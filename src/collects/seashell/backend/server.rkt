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

  ;; Message handler.
  (define/contract (handle-message message)
    (-> jsexpr? jsexpr?)
    (match message
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
           [`(hash-table
               (id ,id)
               (type "getListing"))
             `#hash((id . ,id) (result . "unimplemented"))]))

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

    ;; TODO handle id field in JSON object.
    ;(match message
    ;  [`(hash-table
    ;     ('type "runProgram")
    ;     ('name ,name))]
    ;  [`(hash-table
    ;     ('type "compileProgram")
    ;     ('name ,name))]
    ;  [`(hash-table
    ;     ('type "getListing")
    ;     ('project ,project))]
    ;  [`(hash-table
    ;     ('type "loadFile")
    ;     ('project ,project)
    ;     ('name ,name))]
    ;  [`(hash-table
    ;     ('type "saveFile")
    ;     ('project ,project)
    ;     ('name ,name)
    ;     ('contents ,contents))]
    ;  [`(hash-table
    ;     ('type "revertFile")
    ;     ('project ,project)
    ;     ('name ,name))])

    (logf 'info "Received message: ~s~n" data)
    (main-loop connection state))

  (define (conn-dispatch key wsc header-resp)
    (ws-send wsc #"hello Seashell/0")
    (main-loop wsc 'unused key))

  ;; EXECUTION BEGINS HERE

  (file-stream-buffer-mode (current-input-port) 'none)
  (file-stream-buffer-mode (current-output-port) 'none)

  (define key (seashell-crypt-key-server-read (current-input-port)))

  (define conf-chan  (make-async-channel))

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
  (shutdown-server)
  (logf/sync 'info "Graceful shutdown."))
