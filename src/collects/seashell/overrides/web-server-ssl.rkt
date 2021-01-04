#lang racket/base
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

;; This file was modified from ssl-unit-tcp.rkt in Racket 5.3.6
(require openssl racket/unit web-server/private/dispatch-server-sig)
(provide pfs:make-ssl-connect@)

(define (pfs:make-ssl-connect@ server-cert-file server-key-file
         [ciphers "DEFAULT:!aNULL:!eNULL:!LOW:!EXPORT:!SSLv2"]
         [ecdhe-curve 'secp521r1]
         [dhe-param-path ssl-dh4096-param-bytes])
  (define the-ctxt
    (ssl-make-server-context))
  (ssl-load-certificate-chain! the-ctxt server-cert-file)
  (ssl-load-private-key! the-ctxt server-key-file)
  (when ciphers
    (ssl-set-ciphers! the-ctxt ciphers))
  (when ecdhe-curve
    (ssl-server-context-enable-ecdhe! the-ctxt ecdhe-curve))
  (when dhe-param-path
    (ssl-server-context-enable-dhe! the-ctxt dhe-param-path))
  (define-unit ssl:dispatch-server-connect@
    (import) (export dispatch-server-connect^)
    (define (port->real-ports ip op)
      (ports->ssl-ports	ip op
                        #:mode 'accept
                        #:context the-ctxt)))
  ssl:dispatch-server-connect@)
