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

;; This file was modified from net/websocket/client.rkt frm Racket 5.3.6
(require typed/net/url
         typed/web-server/http
         seashell/websocket/connection
         seashell/websocket/handshake
         typed/openssl
         seashell/overrides/typed-web-server
         seashell/utils)
(require/typed web-server/http/response
               [print-headers (-> Output-Port (Listof Header) Void)])
(require/typed web-server/http/request
               [read-headers (-> Input-Port (Listof Header))])

;; wss-url? u
;; Is u a Secure WebSockets URL?
(: wss-url? (-> Any Boolean))
(define (wss-url? u)
  (and (url? u)
       (equal? (url-scheme u) "wss")))

;; ws-url?
;; Is u a WebSockets URL?
(: ws-url? (-> Any Boolean))
(define (ws-url? u)
  (and (url? u)
       (or (equal? (url-scheme u) "ws")
           (wss-url? u))))

;; ws-connect url [#headers]
;; Connects to the specified websocket url.
(: ws-connect (->* (URL) (#:headers (Listof Header)) Websocket-Connection))
(define (ws-connect url #:headers [headers '()])
  (unless (ws-url? url)
    (raise (exn:fail:contract "ws-connect: was not passed ws(s):// URL."
                              (current-continuation-marks))))
  ;; Grab the absolute path / host / port.
  (define host (cast (url-host url) String))
  (define port (cast (url-port url) Positive-Integer))
  (define upath (url-path url))
  (define the-path
    (if (null? upath)
        "/"
        (let ([pre-path
               (add-between
                (map (λ ([pp : path/param])
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
  (define connect #{(if (wss-url? url) ssl-connect tcp-connect) :: (-> String Positive-Integer (Values Input-Port Output-Port))})
  (define-values (ip op) (connect host port))
  ;; Handshake (client)
  (define handshake (make-handshake))
  (write-bytes (call-with-output-bytes
                (λ ([op : Output-Port])
                  (fprintf op "GET ~a HTTP/1.1\r\n" the-path)
                  (print-headers
                   op
                   (list* (header #"Host" (string->bytes/utf-8 host))
                          (header #"Connection" #"Upgrade")
                          (header #"Upgrade" #"WebSocket")
                          (header #"Sec-WebSocket-Key" handshake)
                          headers))))
               op)
  (flush-output op)
  ;; Handshake (server).
  (define sresponse (check-eof (read-bytes-line ip 'any)))
  (unless (and sresponse (regexp-match #"HTTP/1.1 101 .*" sresponse))
    (raise (exn:websocket
            (format "Invalid server response line.  Got ~e." sresponse)
            (current-continuation-marks))))
  
  (define rheaders (read-headers ip))
  (define server-ans (headers-assq* #"Sec-WebSocket-Accept" rheaders))
  ;; Check the handshake.
  (unless (and server-ans (handshake-solution-ok? handshake (header-value server-ans)))
    (raise (exn:websocket
            (format "Invalid server handshake response. Got ~e for ~e." server-ans handshake)
            (current-continuation-marks))))
  ;; OK, make connection.
  (make-ws-connection
        ip op
        (make-ws-control)
        #"GET" url rheaders #t))
