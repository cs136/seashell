#lang racket
;; Seashell's websocket library.
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
(require openssl/sha1
         net/base64
         unstable/contract
         web-server/private/dispatch-server-unit
         web-server/private/dispatch-server-sig
         web-server/private/connection-manager
         web-server/http/response
         web-server/http/request
         web-server/http/request-structs
         net/tcp-sig
         (prefix-in raw: net/tcp-unit)
         racket/async-channel)



;; handshake-solution key
;; Calculates the correct WebSocket handshake response to key.
;;
;; Arguments:
;;  key - Client's handshake.
;; Returns:
;;  Handshake response.
(define/contract (handshake-solution key)
  (-> bytes? bytes?)
  (base64-encode
   (with-input-from-bytes
    (bytes-append key #"258EAFA5-E914-47DA-95CA-C5AB0DC85B11")
    (lambda() (sha1-bytes (current-input-port))))
   #""))


;; (seashell-websocket-serve conn-dispatch ...)
;; Starts a dispatching WebSocket server compliant with 
;; RFC 6455.
;;
;; Arguments:
;;   Consult http://docs.racket-lang.org/net/websocket.html
;; Returns:
;;   A thunk when invoked stops the Dispatching Server.
(define/contract 
  (seashell-websocket-serve 
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
  (define (dispatch c _)
    ;; Grab the in/out ports.
    (define ip (connection-i-port c))
    (define op (connection-o-port c))
    ;; Read the first line.
    (define cline (read-bytes-line ip 'any))
    ;; Get headers.
    (define headers (read-headers ip))
    (define keyh (headers-assq* #"Sec-WebSocket-Key" headers))
    (unless keyh (error 'seashell-websocket-serve "Invalid WebSocket request, no Key"))
    (define key (header-value keyh))
    (define proth (headers-assq* #"Sec-WebSocket-Protocol" headers))
    (define prot (if proth (header-value proth) #f))
    
    ;; Compute custom headers.
    (define-values (conn-headers state) (pre-conn-dispatch cline headers))
    
    ;; Write headers.
    (fprintf op "HTTP/1.1 101 Switching Protocols\r\n")
    (print-headers
     op
     (list* (make-header #"Upgrade" #"WebSocket")
            (make-header #"Connection" #"Upgrade")
            (make-header #"Server" #"Seashell/0")
            (make-header #"Sec-Websocket-Accept" (handshake-solution key))
            conn-headers))
    ;; ... more headers.
    (when prot
      (print-headers
       op
       (list (make-header #"Sec-WebSocket-Protocol" prot))))
    
    ;; Flush headers out before:
    (flush-output op)
    
    ;; Starting output.
    (define conn
      (values #f cline conn-headers ip op))
    (conn-dispatch conn state))
  1)

