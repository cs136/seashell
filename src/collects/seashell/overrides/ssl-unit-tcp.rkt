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
(provide make-ssl-tcp@)
(require racket/unit net/tcp-sig
         openssl
         (prefix-in tcp: racket/tcp))

(define (error/network who fmt . args)
  (raise (make-exn:fail:network
          (format "~a: ~a" who (apply format fmt args))
          (current-continuation-marks))))

(define (make-ssl-tcp@
         server-cert-file server-key-file server-root-cert-files
         server-suggest-auth-file
         client-cert-file client-key-file client-root-cert-files
         [ciphers "DEFAULT:!aNULL:!eNULL:!LOW:!EXPORT:!SSLv2"]
         [ecdhe-curve 'secp521r1]
         [dhe-param-path ssl-dh4096-param-path])
 ;; Load contexts first.
 (define ctx (ssl-make-client-context))
 (define sctx (ssl-make-server-context))

 (when server-cert-file
   (ssl-load-certificate-chain! sctx server-cert-file))
 (when server-key-file
   (ssl-load-private-key! sctx server-key-file))
 (when server-root-cert-files
   (ssl-set-verify! sctx #t)
   (map (lambda (f)
          (ssl-load-verify-root-certificates! sctx f))
        server-root-cert-files))
 (when server-suggest-auth-file
   (ssl-load-suggested-certificate-authorities! sctx server-suggest-auth-file))

 (when client-cert-file
   (ssl-load-certificate-chain! ctx client-cert-file))
 (when client-key-file
   (ssl-load-private-key! ctx client-key-file))
 (when client-root-cert-files
   (ssl-set-verify! ctx #t)
   (map (lambda (f)
          (ssl-load-verify-root-certificates! ctx f))
        client-root-cert-files))

 (when ciphers
   (ssl-set-ciphers! ctx ciphers)
   (ssl-set-ciphers! sctx ciphers))

 (when ecdhe-curve
   (ssl-server-context-enable-ecdhe! sctx ecdhe-curve))
 (when dhe-param-path
   (ssl-server-context-enable-dhe! sctx dhe-param-path))

  (unit
   (import)
   (export tcp^)

   (define (tcp-abandon-port p)
     (if (input-port? p)
       (close-input-port p)
       (close-output-port p)))

   (define (tcp-accept listener)
     (define-values (ip op) (tcp:tcp-accept listener))
     (ports->ssl-ports ip op
                       #:mode 'accept
                       #:context sctx
                       #:close-original? #t
                       #:error/ssl error/network))

   (define (tcp-accept/enable-break listener)
     (define-values (ip op) (tcp:tcp-accept/enable-break listener))
     (ports->ssl-ports ip op
                       #:mode 'accept
                       #:context sctx
                       #:close-original? #t
                       #:error/ssl error/network))

   ;; accept-ready? doesn't really work for SSL:
   (define (tcp-accept-ready? p)
     #f)

   (define (tcp-addresses port [port-numbers? #f])
     (cond
       [(tcp:tcp-listener? port)
        (tcp:tcp-addresses port port-numbers?)]
       [else
        (ssl-addresses port port-numbers?)]))

   (define (tcp-close port)
     (cond
       [(tcp:tcp-listener? port)
        (tcp:tcp-close port)]
       [else
        (ssl-close port)]))

   (define (tcp-connect hostname port-k)
     (ssl-connect hostname port-k ctx))
   (define (tcp-connect/enable-break hostname port-k)
     (ssl-connect/enable-break hostname port-k ctx))

   (define tcp-listen tcp:tcp-listen)

   (define tcp-listener? ssl-listener?)))
