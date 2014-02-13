#lang racket/base
;; Seashell's websocket library.
;; Copyright (C) 2013-2014 The Seashell Maintainers.
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
(require net/base64
         net/tcp-sig
         openssl/sha1
         (prefix-in raw: net/tcp-unit)
         racket/async-channel
         racket/contract
         racket/list
         racket/port
         racket/unit
         unstable/contract
         web-server/http/request
         web-server/http/request-structs
         web-server/http/response
         web-server/private/connection-manager
         web-server/private/dispatch-server-sig
         web-server/private/dispatch-server-unit
         seashell/websocket/connection
         seashell/websocket/handshake)

(provide ws-serve)



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
   #:conn-headers [pre-conn-dispatch (λ (cline hs) (values empty (void)))]
   #:tcp@ [tcp@ raw:tcp@]
   #:port [port 80]
   #:listen-ip [listen-ip #f]
   #:max-waiting [max-waiting 4]
   #:timeout [initial-connection-timeout (* 60 60)]
   #:confirmation-channel [confirm-ch #f])
  (->* ((any/c any/c . -> . void))
       (#:conn-headers
        (bytes? (listof header?) . -> . (values (listof header?) any/c))
        #:tcp@
        (unit/c (import) (export tcp^))
        #:port
        tcp-listen-port?
        #:listen-ip
        (or/c string? false/c)
        #:max-waiting
        integer?
        #:timeout
        integer?
        #:confirmation-channel
        (or/c false/c async-channel?))
       (-> void))
  (define (read-request c p port-addresses)
    (values #f #t))
  (define (dispatch c _)
    ;; Grab the in/out ports.
    (define ip (connection-i-port c))
    (define op (connection-o-port c))
    ;; Read the first line.
    (define cline (read-bytes-line ip 'any))
    ;; Get headers.
    (define headers (read-headers ip))
    (define keyh (headers-assq* #"Sec-WebSocket-Key" headers))
    (unless keyh (raise 
                  (exn:websocket 
                   "Invalid WebSocket request, no Key"       
                   (current-continuation-marks))))
    (define key (header-value keyh))
    
    ;; Compute custom headers.  This function also handles subprotocols.
    (define-values (conn-headers state) (pre-conn-dispatch cline headers))

    ;; Write headers.
    (fprintf op "HTTP/1.1 101 Switching Protocols\r\n")
    (print-headers
     op
     (list* (make-header #"Upgrade" #"WebSocket")
            (make-header #"Connection" #"Upgrade")
            (make-header #"Server" #"Seashell/0")
            (make-header #"Sec-Websocket-Accept" (handshake-solution-server key))
            conn-headers))

    ;; Flush headers out before:
    (flush-output op)

    ;; Starting output.
    (define conn
      (make-ws-connection
        ip op
        (make-ws-control)
        cline conn-headers #f))

    (conn-dispatch conn state))
  (define-unit-binding a-tcp@
    tcp@ (import) (export tcp^))
  (define-compound-unit/infer dispatch-server@/tcp@
    (import dispatch-server-config^)
    (link a-tcp@ dispatch-server@)
    (export dispatch-server^))
  (define-values/invoke-unit
    dispatch-server@/tcp@
    (import dispatch-server-config^)
    (export dispatch-server^))
  (serve #:confirmation-channel confirm-ch))

