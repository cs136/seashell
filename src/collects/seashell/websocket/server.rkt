#lang typed/racket
;; Seashell's websocket library.
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
(require typed/net/url
         seashell/overrides/typed-web-server
         seashell/websocket/connection
         seashell/websocket/handshake)

(provide make-websocket-dispatcher)

;; make-websocket-dispatcher -> dispatcher/c
;; Makes a RFC 6455 websocket dispatcher.
;;
;; Arguments:
;;   Consult http://docs.racket-lang.org/net/ws.html
;; Returns:
;;   Dispatcher function.
(: make-websocket-dispatcher (All (X)
                                  (->* ((-> Websocket-Connection X Any)
                                        #:conn-headers (-> Bytes URL (Listof Header)
                                                           (Values (U Boolean String) (Listof Header) X)))
                                       ()
                                       Dispatcher)))
(define (make-websocket-dispatcher
                   dispatch
                   #:conn-headers pre-conn-dispatch)
  (lambda ([connection : Connection] [request : Request])
    ;; Grab the in/out ports.
    (define ip (connection-i-port connection))
    (define op (connection-o-port connection))
    ;; Get the headers.
    (define headers (request-headers/raw request))

    ;; Get WebSocket negotiation.
    (define keyh (let
                   ([x : (U False Header) (headers-assq* #"Sec-WebSocket-Key" headers)])
                   (cond
                     [(not x) (next-dispatcher)]
                     [else x])))
    ;; Get the key.
    (define key (header-value keyh))

    ;; Compute custom headers.  This function also handles subprotocols.
    (define-values (ok? conn-headers state) (pre-conn-dispatch (request-method request)
                                                               (request-uri request)
                                                               headers))

    ;; Write headers.
    (cond
      [(or (string? ok?) (not ok?))
        (fprintf op "HTTP/1.1 500 Internal Server Error\r\n")
        (print-headers op
          (list* (header #"Server" #"Seashell/WebSocket")
                  conn-headers))
        (fprintf op "\r\n")
        (when (string? ok?)
          (fprintf op "~a" ok?))
        (flush-output op)
        (void)]
      [else
        (fprintf op "HTTP/1.1 101 Switching Protocols\r\n")
        (print-headers
         op
         (list* (header #"Upgrade" #"WebSocket")
                (header #"Connection" #"Upgrade")
                (header #"Server" #"Seashell/WebSocket")
                (header #"Sec-Websocket-Accept" (handshake-solution-server key))
                conn-headers))

        ;; Flush headers out before:
        (flush-output op)

        ;; Kill the connection timer.
        (cancel-timer! (connection-timer connection))

        ;; Starting output.
        (define ws-conn
          (make-ws-connection
            ip op
            (make-ws-control)
            (request-method request)
            (request-uri request)
            (request-headers/raw request)
            #f))

        ;; Drop to the real dispatch function.
        (dispatch ws-conn state)
        (void)])))
