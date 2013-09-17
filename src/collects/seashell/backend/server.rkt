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

(define (backend-main)
  ;; Log / handlers setup.
  (make-port-logger "^info$" (current-error-port))
  (make-port-logger "^warn$" (current-error-port))
  (make-port-logger "^exception$" (current-error-port))

  (define ss-exn-handler
    (lambda(e)
      (when (not (exn:break? e))
        (if (read-config 'debug)
            (logf/sync 'exception "~a:~ntrace: ~a"
              (exn-message e)
              (foldl string-append ""
                    (format-stack-trace
                      (continuation-mark-set->context
                      (exn-continuation-marks e)))))
            (logf/sync 'exception
                       "Encountered an exception. Turn debug mode on for information [insecure].")))
      ((error-escape-handler))))

  ;; TODO
  ;(uncaught-exception-handler ss-exn-handler)

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
    (-> (and/c jsexpr?)
        (and/c jsexpr?))
    (cond
      [(or (not (hash? message)) (not (hash-has-key? message 'id)))
        `#hash((id . (json-null)) (result . (format "Bad message: ~s" message)))]
      [else
        (define id (hash-ref message 'id))
        (with-handlers
          ([exn:project?
            (lambda (exn)
              `#hash((id . ,id)
                     (error . #t)
                     (result . ,(exn-message exn))))]
           [exn:fail:contract?
            (lambda (exn)
              `#hash((id . ,id)
                     (error . #t)
                     (result . ,(format "Bad argument: ~a." (exn-message exn)))))]
           [exn:git?
            (lambda (exn)
              `#hash((id . ,id)
                     (error . #t)
                     (result .
                      ,(format "Internal [git] error: ~s." (exn-message exn)))))]
           ;; TODO - other handlers here.
           )
           (match message
                  ;; Project compilation functions.
                  [`(hash-table
                     (id ,id)
                     (type "runProgram")
                     (name ,name))
                    `#hash((id . ,id) (result . "unimplemented"))]
                  [`(hash-table
                     (id ,id)
                     (type "compileProgram")
                     (name ,name))
                    `#hash((id . ,id) (result . "unimplemented"))]
                  ;; Project manipulation functions.
                  [`(hash-table
                     (id ,id)
                     (type . "getProjects"))
                    `#hash((id . ,id)
                           (result . ,(list-projects)))]
                  [`(hash-table
                     (id . ,id)
                     (type . "listProject")
                     (project . ,project))
                    `#hash((id . ,id) (result . ,(list-files project)))]
                  [`(hash-table
                     (id . ,id)
                     (type . "newProject")
                     (project . ,project))
                    (new-project project)
                    `#hash((id . ,id) (result . #t))]
                  [`(hash-table
                     (id . ,id)
                     (type . "deleteProject")
                     (project . ,project))
                    (delete-project project)
                    `#hash((id . ,id) (result . #t))]
                  [`(hash-table
                     (id . ,id)
                     (type . "saveProject")
                     (project . ,project))
                    (save-project project)
                    `#hash((id . ,id) (result . #t))]
                  ;; File functions.
                  [`(hash-table
                     (id . ,id)
                     (type . "newFile")
                     (project . ,project)
                     (file . ,file))
                    (new-file project file)
                    `#hash((id . ,id) (result . #t))]
                  [`(hash-table
                     (id . ,id)
                     (type . "deleteFile")
                     (project . ,project)
                     (file . ,file))
                    (delete-file project file)
                    `#hash((id . ,id) (result . #t))]
                  [`(hash-table
                     (id . ,id)
                     (type . "writeFile")
                     (project . ,project)
                     (file . ,file)
                     (contents . ,contents))
                    (write-file project file (string->bytes/utf-8 contents))
                    `#hash((id . ,id) (result . #t))]
                  [`(hash-table
                     (id . ,id)
                     (type . "readFile")
                     (project . ,project)
                     (file . ,file))
                    `#hash((id . ,id) (result . ,(bytes->string/utf-8 read-file project file)))]
                  ;; TODO: revertFile.
                  ;; Fall through case.
                  [`(hash-table
                     (id . ,id)
                     (key . ,value) ...)
                    `#hash((id . ,id)
                           (error . #t)
                           (result . (format "Unknown message: ~s" message)))]))]))

  ;; Channel used to keep process alive.
  (define keepalive-chan (make-async-channel))

  ;; Per-connection event loop.
  (define (main-loop connection state key)
    ;; TODO - probably want to sync here also on a CLOSE frame.
    ;; TODO - close the connection when appropriate (timeout).
    ;; TODO - send messages on the keepalive channel whenever there is activity.
    (define data (ws-recv connection))

    ;; Framing format (all binary bytes):
    ;; [IV - 12 bytes][GCM tag - 16 bytes][1 byte - Auth Len][Auth Plain][Encrypted Frame]
    (define iv (subbytes data 0 12))
    (define tag (subbytes data 12 28))
    (define authlen (bytes-ref data 28))
    (define auth (subbytes data 29 (+ 29 authlen)))
    (define encrypted (subbytes data (+ 29 authlen)))
    (define plain (seashell-decrypt key iv tag encrypted auth))

    ;; Parse plain as a JSON message.
    (define message (bytes->jsexpr plain))
    (logf 'info "Received message: ~s~n" message)

    ;; Put long running computation in a deferred thread/future.
    ;; This will require synchronization in places that expect it - probably FFI things will need this.
    (future
      (lambda ()
        (define result (handle-message message))
        (define-values
          (iv coded tag)
          (seashell-encrypt key (jsexpr->bytes result) #""))
        (ws-send connection (bytes-append iv tag (bytes 0) #"" coded))))
    (main-loop connection state))

  (define (conn-dispatch key wsc header-resp)
    (define-values
      (iv coded tag)
      (seashell-encrypt key (jsexpr->bytes '("hello Seashell/0")) #""))
    (ws-send wsc (bytes-append iv tag (bytes 0) #"" coded))
    (main-loop wsc 'unused key))

  ;; EXECUTION BEGINS HERE

  (file-stream-buffer-mode (current-input-port) 'none)
  (file-stream-buffer-mode (current-output-port) 'none)

  (define key (seashell-crypt-key-server-read (current-input-port)))
  (define conf-chan  (make-async-channel))
  (init-projects)

  (define shutdown-server
    (seashell-websocket-serve
      ((curry conn-dispatch) key)
      #:port 0
      #:listen-ip "0.0.0.0"
      #:max-waiting 4
      #:timeout (* 60 60)
      #:confirmation-channel conf-chan))

  (define start-result (async-channel-get conf-chan))

  (when (exn? start-result)
    (raise start-result))

  (printf "~a~n" start-result)

  (with-handlers
    ([exn:break? (lambda(e) (logf/sync 'exception "Terminating on break~n"))])
    (let loop ()
      (define timeout-alarm (alarm-evt (+ (current-inexact-milliseconds)
                                          (read-config 'backend-client-idle-timeout))))
      (match (sync/enable-break timeout-alarm keepalive-chan)
        [(? async-channel?) (loop)])))

  ;; Shutdown.
  (logf/sync 'info "Shutting down...")
  (shutdown-server)
  (logf/sync 'info "Graceful shutdown.")
  (exit 0))
