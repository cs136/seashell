#lang racket
;; Seashell's backend server.
;; Copyright (C) 2013 The Seashell Maintainers.
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
(require seashell/crypto
         seashell/websocket
         seashell/log
         seashell/seashell-config
         seashell/format-trace
         seashell/backend/project
         seashell/backend/files
         seashell/backend/runner
         seashell/git
         racket/async-channel
         json)

(provide conn-dispatch)

;; Dispatch function.
;; Arguments:
;;  key - Communications key.
;;  wsc - WebSocket connection.
;;  header-resp - Headers.
;;  keepalive-chan - Keepalive channel.
(define (conn-dispatch key keepalive-chan connection header-resp)
  (define authenticated? #f)
 
  ;; (send-message connection message) -> void?
  ;; Sends a JSON message, by converting it to a bytestring.
  ;;
  ;; Arguments:
  ;;  connection - Websocket connection.
  ;;  message - Seashell message, as a JSON expression.
  (define/contract (send-message connection message)
    (-> ws-connection? jsexpr? void?)
    (ws-send connection (jsexpr->bytes message)))

  ;; (project-output-runner-thread)
  ;; Helper thread for dealing with output from running
  ;; projects.
  (define (project-runner-thread pid)
    (define stdout (program-stdout pid))
    (define stderr (program-stderr pid))
    (define runtime-stderr (program-runtime-stderr pid))
    (define wait-evt (program-wait-evt pid))
    
    (let loop ()
      (match (sync wait-evt stdout stderr runtime-stderr)
             [(? (lambda (evt) (eq? evt wait-evt)))
              ;; Program quit
              (define message
                `#hash((id . -3)
                       (pid . ,pid)
                       (result . 
                         #hash((type . "done")
                               (status . (program-status pid))))))
              (send-message connection message)]
             [(? (lambda (evt) (eq? evt stdout)))
              ;; Read line from port, write it back out
              (define line (read-line stdout))
              (define message
                `#hash((id . -3)
                       (pid . ,pid)
                       (result .
                         #hash((type . "stdout")
                               (message . line)))))
              (send-message connection message)]
             [(? (lambda (evt) (eq? evt stderr)))
              ;; Read line from port, write it back out
              (define line (read-line stderr))
              (define message
                `#hash((id . -3)
                       (pid . ,pid)
                       (result .
                         #hash((type . "stderr")
                               (message . line)))))
              (send-message connection message)]
             [(? (lambda (evt) (eq? evt runtime-stderr)))
              ;; TODO: Parse messages here and save
              (define line (read-line runtime-stderr))
              (define message
                `#hash((id . -3)
                       (pid . ,pid)
                       (result .
                         #hash((type . "stderr")
                               (message . line)))))
              (send-message connection message)]
      )))

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
      ;; Project compilation functions.
      [(hash-table
        ('id id)
        ('type "runProgram")
        ('name name))
       (define pid (run-project name))
       `#hash((id . ,id)
              (success . #t)
              (result . pid))]
      [(hash-table
        ('id id)
        ('type "compileProgram")
        ('name name))
       (define-values (result messages)
         (compile-project name))
       `#hash((id . ,id)
              (success . ,result)
              (result . ,messages))]
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
        ('type "deleteProject")
        ('project project))
       (delete-project project)
       `#hash((id . ,id)
              (success . #t)
              (result . #t))]
      [(hash-table
        ('id id)
        ('type  "saveProject")
        ('project project))
       (save-project project)
       `#hash((id . ,id)
              (success . #t)
              (result . #t))]
      ;; File functions.
      [(hash-table
        ('id id)
        ('type "newFile")
        ('project project)
        ('file file))
       (new-file project file)
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
        ('type "writeFile")
        ('project project)
        ('file file)
        ('contents contents))
       (write-file project file (string->bytes/utf-8 contents))
       `#hash((id . ,id)
              (success . #t)
              (result . #t))]
      [(hash-table
        ('id id)
        ('type "readFile")
        ('project project)
        ('file file))
       `#hash((id . ,id)
              (success . #t)
              (result . ,(bytes->string/utf-8 (read-file project file))))]
      ;; TODO: revertFile.
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
        ('type "serverAuth"))
       ;; This does server authentication.
       ;; We send some plaintext that is encrypted with the shared secret.
       ;; The client should verify that the message is as expected before proceeding.
       (define-values (iv coded tag)
         (seashell-encrypt key (seashell-crypt-make-token) #""))
       `#hash((id . ,id)
              (success . #t)
              (result . (,(bytes->list iv) ,(bytes->list coded) ,(bytes->list tag))))]
      [(hash-table
        ('id id)
        ('type "clientAuth")
        ('data (list iv coded tag)))
       ;; This does client authentication.
       ;; Try and decode their encrypted data, and verify integrity.
       ;; If it succeeds, authenticated? <- #t and
       ;; return success.
       ;; Otherwise, fail them.
       (with-handlers
           ([exn:crypto?
             (lambda (exn)
               `#hash((id . ,id)
                      (success . #f)))])
         (logf 'debug
               "Got authenticated bytes: ~s"
               (bytes->list (seashell-decrypt
          key
          (apply bytes iv)
          (apply bytes tag)
          (apply bytes coded)
          #"")))
         ;; We don't actually care what they sent.
         ;; We just care that it decoded properly.
         (set! authenticated? #t)
         `#hash((id . ,id)
                (success . #t)))]
      [_
       `#hash((id . ,(hash-ref message 'id))
              (success . #f)
              (result . ,(format "Unknown message: ~s" message)))]
      )
    )
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
            [exn:git?
             (lambda (exn)
               `#hash((id . ,id)
                      (success . #f)
                      (result .
                              ,(format "Internal [git] error: ~s." (exn-message exn)))))])
         (cond
           [authenticated?
            (dispatch-authenticated message)]
           [else
            (dispatch-unauthenticated message)]
           ))]))
  
  ;; Per-connection event loop.
  (define (main-loop)
    (with-handlers
        ([exn:websocket?
          (lambda (exn)
            (logf 'error (format "Data connection failure: ~a" (exn-message exn))))])
      (logf 'debug "In main loop.")
      
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
         ;; This needs to run in blocking mode.
         (define message (bytes->jsexpr data))
         (thread
          (lambda ()
            (async-channel-put keepalive-chan "[...] And we're out of beta.  We're releasing on time.")
            (define result (handle-message message))
            (logf 'debug "Result of handling message ~s: ~s" message result)
            (send-message connection result)))
         (main-loop)])))
  
  (logf 'info "Received new connection.")
  (send-message connection `#hash((id . -1)
                                  (success . #t)
                                  (result . "Hello from Seashell/0!")))
  (main-loop))
