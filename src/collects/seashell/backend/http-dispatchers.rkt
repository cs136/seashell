#lang racket
;; Seashell's backend server.
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
(require
  net/url
  seashell/log
  seashell/seashell-goncifg
  web-server/http/xexpr
  web-server/http/request-structs
  (prefix-in lift: web-server/dispatchers/dispatch-lift))

(provide request-logging-dispatcher standard-error-dispatcher)

;; (request-logging-dispatcher) -> (void?)
;; Dispatcher that logs all incoming requests to the standard log.
(define (request-logging-dispatcher connection request)
  (logf 'info "~a ~a from ~a." (request-method request) (url->string (request-uri request))
        (request-client-ip request))
  (next-dispatcher))

;; (standard-error-page request?) -> response?
;; Sends the standard Seashell error page.
(define (standard-error-page request)
  (response/xexpr
     `(html
        (head
          (title "404 Not Found"))
        (body
          (h1 "Not Found")
          (p  ,(format "The requested URL ~a was not found on this server." (url->string (request-uri request))))
          (hr)
          (address ,(format "Seashell/1.0 running on Racket ~a on ~a"
                            (version) (request-host-ip request)))))
    #:code 404
    #:message "Not Found"
    #:preamble #"<!DOCTYPE HTML>"))
(define standard-error-dispatcher (lift:make standard-error-page))

;; (standard-unauthenticated-page request)
;; Sends the standard Seashell unauthenticated page.
(define (standard-unauthenticated-page request)
  (response/xexpr
     `(html
        (head
          (title "400 Unauthorized"))
        (body
          (h1 "Unauthorized")
          (p  ,(format "You are not authorized to view the request URL ~a." (url->string (request-uri request))))
          (hr)
          (address ,(format "Seashell/1.0 running on Racket ~a on ~a"
                            (version) (request-host-ip request)))))
    #:code 404
    #:message "Not Found"
    #:preamble #"<!DOCTYPE HTML>"))
(define standard-unauthorized-dispatcher (lift:make standard-unauthorized-page))
