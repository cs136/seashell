#lang racket
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
(require
         net/tcp-sig
         net/url
         (prefix-in raw: net/tcp-unit)
         web-server/web-server
         web-server/dispatchers/dispatch
         web-server/http/request-structs
         web-server/http/response
         web-server/private/connection-manager
         web-server/private/timer
         racket/async-channel
         unstable/contract
         seashell/websocket/connection
         seashell/websocket/handshake)

(provide ws-serve make-websocket-dispatcher)

;; make-websocket-dispatcher -> dispatcher/c
;; Makes a RFC 6455 websocket dispatcher.
;;
;; Arguments:
;;   Consult http://docs.racket-lang.org/net/ws.html
;; Returns:
;;   Dispatcher function.
(define/contract (make-websocket-dispatcher
                   dispatch
                   #:conn-headers [pre-conn-dispatch (lambda (method uri hs) (values empty (void)))])
  (->* ((-> ws-connection? any/c any/c))
       (#:conn-headers
        (bytes? url? (listof header?) . -> . (values (listof header?) any/c)))
       dispatcher/c)
  (lambda (connection request)
    ;; Grab the in/out ports.
    (define ip (connection-i-port connection))
    (define op (connection-o-port connection))
    ;; Get the headers.
    (define headers (request-headers/raw request))
    
    ;; Get WebSocket negotiation.
    (define keyh (headers-assq* #"Sec-WebSocket-Key" headers))
    (unless keyh (next-dispatcher))
    ;; Get the key.
    (define key (header-value keyh))
    
    ;; Compute custom headers.  This function also handles subprotocols.
    (define-values (conn-headers state) (pre-conn-dispatch (request-method request)
                                                           (request-uri request)
                                                           headers))

    ;; Write headers.
    (fprintf op "HTTP/1.1 101 Switching Protocols\r\n")
    (print-headers
     op
     (list* (make-header #"Upgrade" #"WebSocket")
            (make-header #"Connection" #"Upgrade")
            (make-header #"Server" #"Seashell/WebSocket")
            (make-header #"Sec-Websocket-Accept" (handshake-solution-server key))
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
    (dispatch ws-conn state)))

;; (ws-serve conn-dispatch ...)
;; Starts a dispatching WebSocket server compliant with
;; RFC 6455.
;;
;; Arguments:
;;   Consult http://docs.racket-lang.org/net/ws.html
;; Returns:
;;   A thunk when invoked stops the Dispatching Server.
(define/contract
  (ws-serve
   conn-dispatch
   #:conn-headers [pre-conn-dispatch (lambda (method uri hs) (values empty (void)))]
   #:tcp@ [tcp@ raw:tcp@]
   #:port [port 80]
   #:listen-ip [listen-ip #f]
   #:max-waiting [max-waiting 4]
   #:timeout [initial-connection-timeout (* 60 60)]
   #:confirmation-channel [confirm-ch #f])
  (->* ((any/c any/c . -> . void))
       (#:conn-headers
        (bytes? url? (listof header?) . -> . (values (listof header?) any/c))
        #:tcp@
        (unit/c (import) (export tcp^))
        #:port
        tcp-listen-port?
        #:listen-ip
        (or/c string? false/c)
        #:max-waiting
        integer?
        #:confirmation-channel
        (or/c false/c async-channel?))
       (-> void))
  (serve 
    #:dispatch (make-websocket-dispatcher conn-dispatch #:conn-headers pre-conn-dispatch)
    #:confirmation-channel confirm-ch
    #:tcp@ tcp@
    #:port port
    #:listen-ip listen-ip
    #:max-waiting max-waiting))
