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
         seashell/git
         racket/async-channel
         json)
(provide backend-main)
(struct exn:fail:counter exn:fail ())

;; (backend/main)
;; Entry point to the backend server.
;;
;; This function is invoked directly from login-process.c
(define (backend-main)
  ;; Log / handlers setup.
  (current-error-port (open-output-file (build-path (read-config 'seashell) "seashell.log")
                                        #:exists 'append))
  (when (read-config 'debug) (make-port-logger "^debug$" (current-error-port)))
  (make-port-logger "^info$" (current-error-port))
  (make-port-logger "^warning$" (current-error-port))
  (make-port-logger "^error$" (current-error-port))

  ;; Directory setup.
  (init-projects)

  ;; Channel used to keep process alive.
  (define keepalive-chan (make-async-channel))

  ;; Dispatch function.
  ;; Arguments:
  ;;  key - Communications key.
  ;;  wsc - WebSocket connection.
  ;;  header-resp - Headers.
  (define (conn-dispatch key wsc header-resp)
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
                        ,(format "Internal [git] error: ~s." (exn-message exn)))))]
             ;; TODO - other handlers here.
             )
             (match message
                    ;; Project compilation functions.
                    [(hash-table
                       ('id id)
                       ('type "runProgram")
                       ('name name))
                      `#hash((id . ,id)
                             (success . #t)
                             (result . "unimplemented"))]
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
                      (delete-file project file)
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
                             (result . ,(bytes->string/utf-8 read-file project file)))]
                    ;; TODO: revertFile.
                    ;; Fall through case.
                    [_
                      `#hash((id . ,(hash-ref message 'id))
                             (success . #f)
                             (result . ,(format "Unknown message: ~s" message)))]))]))


    ;; (make-counter) -> (() -> int?)
    ;; Makes a counter. (range: 0 - 65535)
    ;;
    ;; Returns:
    ;;  A thunk to invoke to get the next element.
    (define/contract (make-counter)
      (-> (-> integer?))
      (define guard (make-semaphore 1))
      (define counter 0)
      (lambda ()
        (semaphore-wait guard)
        (define result counter)
        (set! counter (remainder (add1 counter) (expt 2 16)))
        (semaphore-post guard)
        result))

    ;; (send-message connection message) -> void?
    ;; Sends a JSON message, by converting it to a bytestring
    ;; and encrypting it, and packaging the result into
    ;; a format that JavaScript can understand.
    ;;
    ;; Arguments:
    ;;  connection - Websocket connection.
    ;;  message - Seashell message, as a JSON expression.
    (define counter/out (make-counter))
    (define send-guard (make-semaphore 1))
    (define/contract (send-message connection message)
      (-> ws-connection? jsexpr? void?)
      (call-with-semaphore send-guard
        (lambda ()
          (define ctr (integer->integer-bytes (counter/out) 2 #f #t))
          ;; Framing format (given in bytes)
          ;; Counter [2 bytes]
          ;; IV      [12 bytes]
          ;; GCM tag [16 bytes]
          ;; Auth Len[1 byte]
          ;; Authenticated Data
          ;; Encrypted Frame
          (logf 'debug "Sending message: ~s with counter ~s" message ctr)
          (define-values
            (iv coded tag)
            (seashell-encrypt key
                              (jsexpr->bytes message)
                              ctr))
          (ws-send connection (bytes-append ctr iv tag (bytes 0) #"" coded)))))

    ;; (decrypt-message message) -> jsexpr?
    ;; Given a JSON bytestring, verifies that the counter is accurate,
    ;; decrypts the message, and returns the result.
    ;;
    ;; Arguments:
    ;;  data - bytestring.
    ;; Result:
    ;;  Message, as a JSON expression.
    ;; Notes:
    ;;  This function is _not_ thread safe.  In particular,
    ;;  this function requires that the order in which
    ;;  frames are received is the order in which they are processed.
    ;;  Use semaphores to guard for this.
    (define counter/in (make-counter))
    (define/contract (decrypt-message data)
      (-> bytes? jsexpr?)
      (define ctr (counter/in))
      ;; Framing format (given in bytes)
      ;; Counter [2 bytes]
      ;; IV      [12 bytes]
      ;; GCM tag [16 bytes]
      ;; Auth Len[1 byte]
      ;; Authenticated Data
      ;; Encrypted Frame
      (define read-ctr (integer-bytes->integer (subbytes data 0 2) #f #t))
      (define iv (subbytes data 2 14))
      (define tag (subbytes data 14 30))
      (define authlen (bytes-ref data 30))
      (define auth (subbytes data 31 (+ 31 authlen)))
      (define encrypted (subbytes data (+ 31 authlen)))
      (logf 'debug "Read (parsed) message: (~s ~s) ~s" ctr read-ctr encrypted)

      ;; Check the counters.
      (unless (equal? read-ctr ctr)
        (raise (exn:fail:counter (format "Frame counter mismatch: ~s ~s" read-ctr ctr)
                                 (current-continuation-marks))))

      (define plain (seashell-decrypt key iv tag encrypted auth))

      ;; Parse plain as a JSON message.
      (define message (bytes->jsexpr plain))
      (logf 'debug "Received message: ~s" message)

      message)


    ;; Per-connection event loop.
    ;;
    ;; Arguments:
    ;;  connection - WebSocket connection.
    ;;  state - Unused for now.
    ;;  key - Communications key.
    (define (main-loop connection state key)
      (with-handlers
        ([exn:fail:counter?
           (lambda (exn)
             (logf 'error (format "Data integrity failed: ~a" (exn-message exn)))
             (send-message connection `#hash((id . -2) (error . #t) (result . "Data integrity check failed!")))
             (ws-close connection))]
         [exn:crypto?
           (lambda (exn)
             (logf 'error (format "Cryptographic failure: ~a" (exn-message exn)))
             ;; This may raise another exception, if the cryptographic failure is caused by lack of 
             ;; random bytes.
             (send-message connection `#hash((id . -2) (error . #t) (result . "Cryptographic failure!")))
             (ws-close connection))]
         [exn:websocket?
           (lambda (exn)
             (logf 'error (format "Data connection failure: ~a" (exn-message exn))))])
        (logf 'debug "In main loop.")

        (define alarm (alarm-evt 
                        (+ (current-inexact-milliseconds) (read-config 'backend-client-connection-timeout))))
        (match (sync connection alarm)
               [(? eof-object?)
                ;; CLOSE frame.  Default control function handles this automatically,
                ;; so just quit.
                (logf 'info (format "Client connection closed gracefully."))
                (void)]
               [(? (lambda (result) (eq? result alarm)))
                ;; Alarm - write out a message and close the connection.
                (logf 'info (format "Client timed out."))
                (send-message connection `#hash((id . -2) (error . #t) (result . "Timeout!")))
                (ws-close connection)]
               [(var data)
                ;; Plain old data.
                ;; This needs to run in blocking mode.
                (define message (decrypt-message data))
                (thread
                  (lambda ()
                    (async-channel-put keepalive-chan "[...] And we're out of beta.  We're releasing on time.")
                    (define result (handle-message message))
                    (logf 'debug "Result of handling message ~s: ~s" message result)
                    (send-message connection result)))
                (main-loop connection state key)])))

    (logf 'info "Received new connection.")
    (send-message wsc `#hash((id . -1) (result . "Hello from Seashell/0!")))
    (main-loop wsc 'unused key))

  (logf/sync 'info "Starting up.")

  ;; Unbuffered mode for I/O ports
  (file-stream-buffer-mode (current-input-port) 'none)
  (file-stream-buffer-mode (current-output-port) 'none)

  ;; Read encryption key.
  (define key (seashell-crypt-key-server-read (current-input-port)))

  ;; Start the server.
  (define conf-chan  (make-async-channel))
  (define shutdown-server
    (ws-serve
      ((curry conn-dispatch) key)
      #:port 0
      #:listen-ip "0.0.0.0"
      #:max-waiting 4
      #:timeout (* 60 60)
      #:confirmation-channel conf-chan))
  (define start-result (async-channel-get conf-chan))
  (when (exn? start-result)
    (raise start-result))

  ;; Write out the listening port
  (printf "~a~n" start-result)
  (logf/sync 'info "Listening on port ~a." start-result)

  ;; Loop and serve requests.
  (with-handlers
    ([exn:break? (lambda(e) (logf/sync 'error "Terminating on break."))])
    (let loop ()
      (match (sync/timeout/enable-break (/ (read-config 'backend-client-idle-timeout) 1000) keepalive-chan)
        [#f (void)]
        [else (loop)])))

  ;; Shutdown.
  (logf/sync 'info "Shutting down...")
  (shutdown-server)
  (logf/sync 'info "Graceful shutdown.")
  (exit 0))
