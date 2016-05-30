#lang racket/base
;; Seashell's backend server.
;; Copyright (C) 2013-2015 The Seashell Maintainers.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; See also 'ADDITIONAL TERMS' at the end of the included LICENSE file.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
(require seashell/backend/authenticate
         seashell/websocket
         seashell/log
         seashell/seashell-config
         seashell/backend/project
         seashell/backend/files
         seashell/backend/runner
         seashell/backend/offline
         racket/async-channel
         racket/serialize
         racket/sandbox
         racket/match
         racket/contract
         racket/port
         racket/string
         json)

(provide conn-dispatch)

;; Dispatch function.
;; Arguments:
;;  keepalive-sema - Keepalive semaphore.
;;  wsc - WebSocket connection.
;;  state - [Unused]
(define (conn-dispatch keepalive-sema connection state)
  (define authenticated? #f)
  (define our-challenge (make-challenge))
  (define thread-to-lock-on (current-thread))
 
  ;; (send-message connection message) -> void?
  ;; Sends a JSON message, by converting it to a bytestring.
  ;;
  ;; Arguments:
  ;;  connection - Websocket connection.
  ;;  message - Seashell message, as a JSON expression.
  (define/contract (send-message connection message)
    (-> ws-connection? jsexpr? void?)
    (ws-send connection (jsexpr->bytes message)))

  ;; (start-pid-io project pid)
  ;; Starts I/O communication with the specified PID
  ;; This is useful as to block any I/O messages
  ;; until after the result of runProject has been handled.
  ;;
  ;; Arguments:
  ;;  project - Name of project (debugging)
  ;;  pid - PID of process
  ;; Returns:
  ;;  Thread representing the communication backend.
  (define (start-pid-io project pid)
    (if (equal? 'test (program-mode pid))
      (project-test-thread project pid)
      (project-runner-thread project pid)))

  ;; (project-test-thread project pid)
  ;; Helper thread for dealing with output from running tests.
  ;; Arguments:
  ;;  project - Name of project
  ;;  pid - PID of process
  ;; Returns:
  ;;  Thread that is running the I/O.
  (define (project-test-thread project pid)
    (thread (lambda ()
      ;; These ports do not need to be closed; they
      ;; are Racket pipes and automatically garbage collected.
      (define stdout (program-stdout pid))
      (define stderr (program-stderr pid))
      (define stdin (program-stdin pid))
      (define wait-evt (program-wait-evt pid))
      (define socket-closed-evt (ws-connection-closed-evt connection))

      ;; Close stderr and stdin, as they are not used in test mode.
      (close-output-port stdin)
      (close-input-port stderr)

      (with-handlers
        ;; Data connection failure.  Quit.
        [(exn:websocket? 
           (lambda (exn)
             (logf 'error "Data connection failure: ~a.  Terminating project ~a (PID ~a)." (exn-message exn)
                   project pid)
             (program-kill pid)
             (program-destroy-handle pid)))]
        ;; Block on stdout and on the websocket.
        (match (sync socket-closed-evt (wrap-evt stdout (compose deserialize read)))
           [(? (lambda (evt) (eq? evt socket-closed-evt)))
            ;; Connection died.
            (program-kill pid)
            (program-destroy-handle pid)]
           [(and result (list pid test-name (and test-res (or "timeout" "killed" "passed")) stdout stderr))
            (send-message connection `#hash((id . -4) (success . #t)
                                            (result . #hash((pid . ,pid) (test_name . ,test-name) (result . ,test-res)))))]
           [(list pid test-name "error" exit-code stderr)
            (send-message connection `#hash((id . -4) (success . #t)
                                           (result . #hash((pid . ,pid) (test_name . ,test-name) (result . "error")
                                                                        (exit_code . ,exit-code)
                                                                        (stderr . ,(bytes->string/utf-8 stderr #\?))))))]
           [(list pid test-name "no-expect" stdout stderr)
            (send-message connection `#hash((id . -4) (success . #t)
                                           (result . #hash((pid . ,pid) (test_name . ,test-name) (result . "no-expect")
                                                           (stdout . ,(bytes->string/utf-8 stdout #\?))
                                                           (stderr . ,(bytes->string/utf-8 stderr #\?))))))]
           [(list pid test-name "failed" diff stderr stdout)
            (send-message connection `#hash((id . -4) (success . #t)
                                           (result . #hash((pid . ,pid) (test_name . ,test-name) (result . "failed")
                                                           (diff . ,(map
                                                              (lambda (x)
                                                                (if (list? x)
                                                                  (map (lambda (y)
                                                                         (if (bytes? y) (bytes->string/utf-8 y #\?)
                                                                           y))
                                                                       x)
                                                                  (bytes->string/utf-8 x #\?)))
                                                              diff))
                                                           (stdout . ,(bytes->string/utf-8 stdout #\?))
                                                           (stderr . ,(bytes->string/utf-8 stderr #\?))))))])))))
  
  ;; (project-output-runner-thread)
  ;; Helper thread for dealing with output from running
  ;; projects.
  ;;
  ;; Arguments:
  ;;  project - Name of project
  ;;  pid - PID of process
  ;; Returns:
  ;;  Thread that is running the I/O processing.
  (define (project-runner-thread project pid)
    ;; These ports do not need to be closed; they
    ;; are Racket pipes and automatically garbage collected.
    (define stdout (program-stdout pid))
    (define stderr (program-stderr pid))
    (define wait-evt (program-wait-evt pid))
    (define socket-closed-evt (ws-connection-closed-evt connection))
   
    ;; Helper function for sending messages
    ;; Arguments:
    ;;  name - name of port.
    ;;  contents - contents to send.
    (define (send-contents-for name contents)
      ;; Read line from port, write it back out
      (unless (eof-object? contents)
        (define message
          `#hash((id . -3)
                 (success . #t)
                 (result .
                   #hash((type . ,name)
                         (pid . ,pid)
                         (message . ,contents)))))
        (send-message connection message)))

    ;; Helper function for wrapping events
    ;; Arguments:
    ;;  tag - tag to give.
    ;;  event - name of event.
    (define (tag-event tag event)
      (wrap-evt
        event
        (lambda (result)
          (list tag result))))
    
    (thread
      (lambda ()
        (let loop ()
          (with-handlers
            ;; Data connection failure.  Quit.
            [(exn:websocket? 
               (lambda (exn)
                 (logf 'error "Data connection failure: ~a.  Terminating project ~a (PID ~a)." (exn-message exn)
                       project pid)
                 (program-kill pid)
                 (program-destroy-handle pid)))]
            (match (sync wait-evt
                         socket-closed-evt
                         ;; Well, this is rather inefficient.  JavaScript
                         ;; only supports UTF-8 (and we don't support any
                         ;; fancy encodings), so unless we want to cause
                         ;; any unexpected character breaks...
                         (if (port-closed? stdout)
                           never-evt
                           (tag-event (list "stdout" stdout) (peek-string-evt 1 0 #f stdout)))
                         (if (port-closed? stderr)
                           never-evt
                           (tag-event (list "stderr" stderr) (peek-string-evt 1 0 #f stderr))))
                   [(? (lambda (evt) (eq? evt socket-closed-evt)))
                    ;; Connection died.
                    (program-kill pid)
                    (program-destroy-handle pid)]
                   [(? (lambda (evt) (eq? evt wait-evt)))
                    ;; Program quit
                    (define message
                      `#hash((id . -3)
                             (success . #t)
                             (result . 
                               #hash((type . "done")
                                     (pid . ,pid)
                                     (status . ,(program-status pid))))))
                    ;; Flush ports.  This will work as the writing side
                    ;; of the pipes will be closed.
                    (define stdout-flush
                      (if (port-closed? stdout)
                        eof
                        (port->string stdout)))
                    (define stderr-flush
                      (if (port-closed? stderr)
                        eof
                        (port->string stderr)))
                    (unless (eof-object? stdout-flush)
                      (logf 'debug "Flushing ~s from ~a of project ~a (PID ~a)." stdout-flush "stdout" project pid)
                      (send-contents-for "stdout" stdout-flush))
                    (unless (eof-object? stderr-flush)
                      (logf 'debug "Flushing ~s from ~a of project ~a (PID ~a)." stderr-flush "stderr" project pid)
                      (send-contents-for "stderr" stderr-flush))
                    ;; Destroy the process
                    (logf 'debug "Instance (PID ~a) of ~a quit." project pid)
                    (program-destroy-handle pid)
                    (send-message connection message)]
                   [`((,tag ,port) ,test)
                    (cond
                      [(not (eof-object? test))
                        (define contents (list->string 
                                           (for/list 
                                             ([i (in-range (read-config 'io-buffer-size))])
                                             #:break (not (char-ready? port))
                                             #:break (eof-object? (peek-char port))
                                             (read-char port))))
                        (send-contents-for tag contents)]
                      [else
                        (if (equal? tag "stdout")
                          (close-input-port stdout)
                          (close-input-port stderr))])
                    (loop)]))))))

  ;; (dispatch-handle-program-input message)
  ;; Helper function for dealing with program input from user.
  (define/contract (dispatch-handle-program-input id pid contents)
    (-> integer? integer? string? jsexpr?)
    (write-string contents (program-stdin pid))
    `#hash((id . ,id)
           (pid . ,pid)
           (success . #t)
           (result . #t)))

  ;; (dispatch-authenticated message)
  ;; Dispatcher in authenticated mode.
  ;;
  ;; Arguments:
  ;;  message - JSON message.
  ;; Returns:
  ;;  JSON result.
  (define/contract (dispatch-authenticated message)
    (-> jsexpr? jsexpr?)
    (match message
      [(hash-table
        ('id id)
        ('type "sync")
        (_ _) ...)
       ;; Sync
       `#hash((id . ,id)
              (result . ,(sync-offline-changes message)))]
      ;; Ping, for timeout checking.
      [(hash-table
        ('id id)
        ('type "ping"))
       `#hash((id . ,id)
              (success . #t)
              (result . #t))]
      ;; Program signalling code.
      [(hash-table
        ('id id)
        ('type "programKill")
        ('pid pid))
       (program-kill pid)
       `#hash((id . ,id)
              (success . #t)
              (result . #t))]
      ;; Project running input
      [(hash-table
        ('id id)
        ('type "programInput")
        ('pid pid)
        ('contents contents))
       (dispatch-handle-program-input id pid contents)]
      ;; Project running functions
      [(hash-table
        ('id id)
        ('type "compileAndRunProject")
        ('project name)
        ('question question)
        ('tests test))
       (define-values (success? result) 
         (compile-and-run-project/use-runner name question test))
       `#hash((id . ,id)
              (success . ,success?)
              (result . ,result))]
      [(hash-table
         ('id id)
         ('type "startIO")
         ('project project)
         ('pid pid))
       (start-pid-io project pid)
       `#hash((id . ,id) (success . #t))] 
      ;; Send EOF to stdin of the program with the given pid
      [(hash-table
        ('id id)
        ('type "sendEOF")
        ('pid pid))
       (close-output-port (program-stdin pid))
       `#hash((id . ,id)
              (success . #t)
              (result . #t))]
      ;; Project manipulation functions.
      [(hash-table
        ('id id)
        ('type "getProjects"))
       `#hash((id . ,id)
              (success . #t)
              (result . ,(list-projects)))]
      [(hash-table
        ('id id)
        ('type "listProject")
        ('project project))
       `#hash((id . ,id)
              (success . #t)
              (result . ,(list-files project)))]
      [(hash-table
        ('id id)
        ('type "newProject")
        ('project project))
       (new-project project)
       `#hash((id . ,id)
              (success . #t)
              (result . #t))]
      [(hash-table
        ('id id)
        ('type "newProjectFrom")
        ('project project)
        ('source source))
        (new-project-from project source)
        `#hash((id . ,id)
               (success . #t)
               (result . #t))]
      [(hash-table
        ('id id)
        ('type "deleteProject")
        ('project project))
       (delete-project project)
       `#hash((id . ,id)
              (success . #t)
              (result . #t))]
      [(hash-table
        ('id id)
        ('type "lockProject")
        ('project project))
       (define locked (lock-project project thread-to-lock-on))
       `#hash((id . ,id)
              (success . ,locked)
              (result . ,(if locked #t "locked")))]
      [(hash-table
        ('id id)
        ('type "forceLockProject")
        ('project project))
       (force-lock-project project thread-to-lock-on)
       `#hash((id . ,id)
              (success . #t)
              (result . #t))]
      [(hash-table
        ('id id)
        ('type "unlockProject")
        ('project project))
       (unlock-project project)
       `#hash((id . ,id)
              (success . #t)
              (result . #t))]
      [(hash-table
        ('id id)
        ('type "downloadProject")
        ('project project))
       `#hash((id . ,id)
              (success . #t)
              (result . ,(make-download-token project)))]
      ;; File functions.
      [(hash-table
         ('id id)
         ('type "newFile")
         ('project project)
         ('file file)
         ('normalize normalize)
         (_ _) ...)
       (define tag (new-file project file
                             (string->bytes/utf-8 (hash-ref message 'contents ""))
                             (string->symbol (hash-ref message 'encoding "raw"))
                             normalize))
       `#hash((id . ,id)
              (checksum . ,tag)
              (success . #t)
              (result . #t))]
      [(hash-table
        ('id id)
        ('type "newDirectory")
        ('project project)
        ('dir dir))
       (new-directory project dir)
       `#hash((id . ,id)
              (success . #t)
              (result . #t))]
      [(hash-table
        ('id id)
        ('type "deleteFile")
        ('project project)
        ('file file))
       (remove-file project file)
       `#hash((id . ,id)
              (success . #t)
              (result . #t))]
      [(hash-table
        ('id id)
        ('type "deleteDirectory")
        ('project project)
        ('dir dir))
       (remove-directory project dir)
       `#hash((id . ,id)
              (success . #t)
              (result . #t))]
      [(hash-table
        ('id id)
        ('type "writeFile")
        ('project project)
        ('file file)
        ('contents contents)
        (_ _) ...)
       (define checksum
         (write-file project file (string->bytes/utf-8 contents) (hash-ref message 'checksum #f)))
       `#hash((id . ,id)
              (success . #t)
              (result . ,checksum))]
      [(hash-table
        ('id id)
        ('type "readFile")
        ('project project)
        ('file file))
       (define-values (data checksum) (read-file project file))
       `#hash((id . ,id)
              (success . #t)
              (result . 
                      #hash((data . ,(bytes->string/utf-8 data))
                            (checksum . ,checksum))))]
      ;; for getting contents of object files in offline mode..
      ;;  should probably just combine this with above call
      ;;  so the backend can deal with the 2 cases and send
      ;;  another flag back
      [(hash-table
        ('id id)
        ('type "readFileBytes")
        ('project project)
        ('file file))
       (define-values (data _) (read-file project file))
       #hash((id . ,id)
             (success . #t)
             (result . ,(bytes->list data)))]
      ;; Download/Upload token functions:
      [(hash-table
        ('id id)
        ('type "getExportToken")
        ('project project))
       `#hash((id . ,id)
              (success . #t)
              (result . ,(make-download-token project)))]
      [(hash-table
        ('id id)
        ('type "getUploadFileToken")
        ('project project)
        ('file file))
       `#hash((id . ,id)
              (success . #t)
              (result . ,(make-file-upload-token project file)))]
      ;; Directory manipulation functions
      [(hash-table
         ('id id)
         ('type "createDirectory")
         ('project project)
         ('directory directory))
       ;; TODO: Create directory
       `#hash((id . ,id)
              (success . #t)
              (result . #t))]
      [(hash-table
         ('id id)
         ('type "renameDirectory")
         ('project project)
         ('directory directory))
       ;; TODO: Rename directory
       `#hash((id . ,id)
              (success . #t)
              (result . #t))]
      [(hash-table
         ('id id)
         ('type "deleteDirectory")
         ('project project)
         ('directory directory))
       ;; TODO: Delete directory
       `#hash((id . ,id)
              (success . #t)
              (result . #t))]
      ;; Renaming file dispatch
      [(hash-table
        ('id id)
        ('type "renameFile")
        ('project project)
        ('oldName old-file)
        ('newName new-file))
       (rename-file project old-file new-file)
       `#hash((id . ,id)
              (success . #t)
              (result . #t))]
      [(hash-table
        ('id id)
        ('type "restoreFileFrom")
        ('project project)
        ('file file)
        ('template template))
       `#hash((id . ,id)
              (success . #t)
              (result . ,(restore-file-from-template project file template)))]
      [(hash-table
        ('id id)
        ('type "getMostRecentlyUsed")
        ('project project)
        ('directory directory))
       `#hash((id . ,id)
              (success . #t)
              (result . ,(get-most-recently-used project directory)))]
      [(hash-table
        ('id id)
        ('type "updateMostRecentlyUsed")
        ('project project)
        ('directory directory)
        ('predicate predicate)
        ('data data))
       (update-most-recently-used project directory predicate data)
       `#hash((id . ,id)
              (success . #t)
              (result . #t))]
      [(hash-table
        ('id id)
        ('type "getFileToRun")
        ('project project)
        ('question question))
       `#hash((id . ,id)
              (success . #t)
              (result . ,(get-file-to-run project question)))]
      [(hash-table
        ('id id)
        ('type "setFileToRun")
        ('project project)
        ('folder folder)
        ('question question)
        ('file file))
       (set-file-to-run project question folder file)
       `#hash((id . ,id)
              (success . #t)
              (result . #t))]
      [(hash-table
        ('id id)
        ('type "saveSettings")
        ('settings settings))
       (with-output-to-file (build-path (read-config 'seashell) "settings.txt")
         (lambda () (write settings)) #:exists 'truncate)
       `#hash((id . ,id)
              (success . #t)
              (result . #t))]
      [(hash-table
        ('id id)
        ('type "getSettings"))
       `#hash((id . ,id)
              (success . #t)
              (result . ,(read-settings)))]
      [(hash-table
        ('id id)
        ('project project)
        ('assn assn)
        ('type "marmosetSubmit")
        ('subdir subdir))
        (marmoset-submit "CS136" assn project subdir)
       `#hash((id . ,id)
              (success . #t)
              (result . #t))]
      [(hash-table
        ('id id)
        ('type "archiveProjects")
        ('location location))
        (archive-projects location)
        `#hash((id . ,id)
               (success . #t)
               (result . #t))]
      ;; Fall through case.
      [_
       `#hash((id . ,(hash-ref message 'id))
              (success . #f)
              (result . ,(format "Unknown message: ~s" message)))]))
  
  ;; (dispatch-unauthenticated)
  ;; Dispatcher in unauthenticated mode.
  (define/contract (dispatch-unauthenticated message)
    (-> jsexpr? jsexpr?)
    (match message
      [(hash-table
        ('id id)
        ('type "clientAuth")
        ('response token))
       ;; This does client authentication.
       ;; Try and decode their encrypted data, and verify integrity.
       ;; If it succeeds, authenticated? <- #t and
       ;; return success.
       ;; Otherwise, fail them.
       (with-handlers
           ([exn:authenticate?
             (lambda (exn)
               `#hash((id . ,id)
                      (success . #f)))])
          (authenticate
            token
            our-challenge)
         (set! authenticated? #t)
         `#hash((id . ,id)
                (success . #t)))]
      [_
       `#hash((id . ,(hash-ref message 'id))
              (success . #f)
              (result . ,(format "Unknown message: ~s" message)))]))

  ;; (handle-message message)
  ;;
  ;; Given a message, passes it on to the appropriate function.
  ;;
  ;; Arguments:
  ;;  message - jsexpr? message/request.
  ;; Returns:
  ;;  Response, as a jsexpr?.
  ;; Notes:
  ;;  This function _SHOULD_ not raise _ANY_ exceptions in
  ;;  the course of normal execution and errors (file does not exist, ...)
  (define/contract (handle-message message)
    (-> jsexpr? jsexpr?)
    (cond
      [(or (not (hash? message)) (not (hash-has-key? message 'id)))
       (logf 'error "Got bad message!")
       `#hash((id . -2) (result . ,(format "Bad message: ~s" message)))]
      [else
       (define id (hash-ref message 'id))
       (with-handlers
           ([exn:project?
             (lambda (exn)
               `#hash((id . ,id)
                      (success . #f)
                      (result . ,(exn-message exn))))]
            [exn:fail:contract?
             (lambda (exn)
               `#hash((id . ,id)
                      (success . #f)
                      (result . ,(format "Bad argument: ~a." (exn-message exn)))))]
            [exn?
             (lambda (exn)
               (logf 'debug "Internal server error: ~a.~n***Stacktrace follows:***~n~a~n***End Stacktrace.***~n" (exn-message exn)
                     (format-stack-trace (exn-continuation-marks exn)))
               `#hash((id . ,id)
                      (success . #f)
                      (result .
                              ,(format "Internal server error: ~a." (exn-message exn)))))])
         (cond
           [authenticated?
            (dispatch-authenticated message)]
           [else
            (dispatch-unauthenticated message)]))]))
  
  ;; Per-connection event loop.
  (define (main-loop)
    (match (sync/timeout (/ (read-config 'backend-client-connection-timeout) 1000) connection)
      [#f
       ;; Alarm - write out a message and close the connection.
       (logf 'info (format "Client timed out."))
       (send-message connection `#hash((id . -2) (error . #t) (result . "Timeout!")))
       (ws-close connection)]
      [(? eof-object?)
       ;; CLOSE frame.  Default control function handles this automatically,
       ;; so just quit.
       (logf 'info (format "Client connection closed gracefully."))
       (void)]
      [(var data)
       ;; Plain old data.
       (thread
        (lambda ()
          (with-handlers
            ([exn:fail:resource? (lambda (exn)
                                   (logf 'error "Memory limits exceeded while processing request!")
                                   (send-message connection `#hash((id . -2)
                                                                   (result . "Request exceeded memory limits."))))]
             [exn:fail? (lambda (exn)
                          (logf 'error "Could not process request!")
                          (send-message connection `#hash((id . -2)
                                                          (result . "Could not process request!"))))])
          (call-with-limits #f (read-config 'request-memory-limit)
            (lambda ()
              (define message (bytes->jsexpr data))
              (semaphore-post keepalive-sema)
              (define result (handle-message message))
              (send-message connection result))))))
       (main-loop)]))
 
  (parameterize
    ([current-subprocess-custodian-mode 'interrupt])
    (with-handlers
        ([exn:websocket?
          (lambda (exn)
            (logf 'error (format "Data connection failure: ~a" (exn-message exn))))])
      (logf 'info "Received new connection.")
      (send-message connection `#hash((id . -1)
                                      (success . #t)
                                      (result . ,our-challenge)))
      (main-loop))))
