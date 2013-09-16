#lang racket
;; Seashell's login gateway.
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
(require net/cgi
         net/url
         seashell/seashell-config
         seashell/log
         seashell/format-trace
         seashell/tunnel
         json)

(provide gateway-main)

;; The gateway CGI expects a username 'u' and password 'p' argument
;; as POST data. It will only run when the webserver reports an SSL
;; connection.
;;  (this is to ensure user has correct Apache config; only SSL
;;   requests should be permitted to this script).

(define (gateway-main)
  (define (response/json jsexpr)
    (output-http-headers)
    (write-json jsexpr)
    (flush-output))
  (define (report-error code desc)
    (response/json `#hash((error . #hash((code . ,code) (message . ,desc)))))
    (exit 0))

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

  ;(uncaught-exception-handler ss-exn-handler)

  (unless
    (equal? (getenv "HTTPS") "on")
    (logf/sync 'warn "Refusing to operate over a non-SSL connection.")
    (report-error 1 "Requires SSL."))

  (unless
    (equal? (get-cgi-method) "POST")
    (report-error 2 "Requires POST request method."))

  (define bdgs (get-bindings))

  (define uname
    (let ((l (extract-bindings 'u bdgs)))
      (unless (= (length l) 1)
        (report-error 3 "Bad username provided."))
      (first l)))

  (define passwd
    (let ((l (extract-bindings 'p bdgs)))
      (unless (= (length l) 1)
        (report-error 3 "Bad password provided."))
      (first l)))

;  (define tun
;    (with-handlers
;      ([exn:tunnel? (lambda(e) (report-error 4 "Session could not be started."))])
      (tunnel-launch uname passwd);))

;  (copy-port (tunnel-out tun) (current-output-port))
  (exit 0))
