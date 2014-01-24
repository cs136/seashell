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

(require racket/tcp
         net/url
         web-server/http/response
         web-server/http/request
         web-server/http/request-structs
         seashell/websocket/connection
         seashell/websocket/handshake
         openssl)

;; wss-url? u
;; Is u a Secure WebSockets URL?
(define/contract (wss-url? u)
  (-> any/c boolean?)
  (and (url? u)
       (equal? (url-scheme u) "wss")))

;; ws-url?
;; Is u a WebSockets URL?
(define (ws-url? u)
  (-> any/c boolean?)
  (and (url? u)
       (or (equal? (url-scheme u) "ws")
           (wss-url? u))))

;; ws-connect url [#headers]
;; Connects to the specified websocket url.
(define/contract (ws-connect url
                             #:headers [headers empty])
  (->* (ws-url?)
       (#:headers (listof header?))
       ws-connection?)

  ;; Grab the absolute path / host / port.
  (define host (url-host url))
  (define port (url-port url))
  (define upath (url-path url))
  (define the-path
    (if (empty? upath)
        "/"
        (let ([pre-path
               (add-between
                (map (λ (pp)
                       (define p (path/param-path pp))
                       (case p
                         [(up) ".."]
                         [(same) "."]
                         [else p]))
                     upath)
                "/")])
          (apply string-append
                 (if (url-path-absolute? url)
                     (list* "/"
                            pre-path)
                     pre-path)))))
  ;; Connect
  (define connect (if (wss-url? url) ssl-connect tcp-connect))
  (define-values (ip op) (connect host port))
  ;; Handshake (client)
  (define handshake (make-handshake))
  (write-bytes (call-with-output-bytes
                (λ (op)
                  (fprintf op "GET ~a HTTP/1.1\r\n" the-path)
                  (print-headers
                   op
                   (list* (make-header #"Host" (string->bytes/utf-8 host))
                          (make-header #"Connection" #"Upgrade")
                          (make-header #"Upgrade" #"WebSocket")
                          (make-header #"Sec-WebSocket-Key" (string->bytes/utf-8 handshake))
                          headers))))
               op)
  (flush-output op)
  ;; Handshake (server).
  (define sresponse (read-bytes-line ip 'any))
  (unless (and sresponse (regexp-match #"HTTP/1.1 101 .*" sresponse))
    (raise (exn:websocket
            (format "Invalid server response line.  Got ~e." sresponse)
            (current-continuation-marks))))
  
  (define rheaders (read-headers ip))
  (define server-ans (headers-assq* #"Sec-WebSocket-Accept" rheaders))
  ;; Check the handshake.
  (unless (handshake-solution-ok? server-ans handshake)
    (raise (exn:websocket
            (format "Invalid server handshake response. Got ~e for ~e." server-ans handshake)
            (current-continuation-marks))))
  ;; OK, make connection.
  (make-ws-connection
        ip op
        (make-ws-control)
        #"" rheaders #t))


(define (freadf ip s)
  (define i (read-line ip 'any))
  (unless (string=? s i)
    (error 'ws-connect "Invalid server response. Expected ~e, got ~e." s i)))
