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
;a;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
(require
  json
  net/url
  seashell/log
  seashell/backend/authenticate
  seashell/backend/project
  seashell/seashell-config
  web-server/http/xexpr
  web-server/http/response-structs
  web-server/http/request-structs
  web-server/dispatchers/dispatch
  web-server/private/connection-manager
  (prefix-in lift: web-server/dispatchers/dispatch-lift))

(provide request-logging-dispatcher
         standard-error-dispatcher
         project-export-dispatcher)

;; (request-logging-dispatcher) -> (void?)
;; Dispatcher that logs all incoming requests to the standard log.
(define/contract (request-logging-dispatcher connection request)
  (-> connection? request? void?)
  (logf 'info "~a ~a from ~a." (request-method request) (url->string (request-uri request))
        (request-client-ip request))
  (next-dispatcher))

;; (standard-error-page request?) -> response?
;; Sends the standard Seashell error page.
(define/contract (standard-error-page request)
  (-> request? response?)
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
    #:message #"Not Found"
    #:preamble #"<!DOCTYPE HTML>"))
(define standard-error-dispatcher (lift:make standard-error-page))

;; (standard-unauthenticated-page request?) -> response?
;; Sends the standard Seashell unauthenticated page.
(define/contract (standard-unauthorized-page request)
  (-> request? response?)
  (response/xexpr
     `(html
        (head
          (title "403 Forbidden"))
        (body
          (h1 "Forbidden")
          (p  ,(format "You are not authorized to view the request URL ~a." (url->string (request-uri request))))
          (hr)
          (address ,(format "Seashell/1.0 running on Racket ~a on ~a"
                            (version) (request-host-ip request)))))
    #:code 403
    #:message #"Forbidden"
    #:preamble #"<!DOCTYPE HTML>"))

;; (standard-server-error-page exn request?) -> response?
;; Sends the standard Seashell server error page.
(define/contract (standard-server-error-page exn request)
  (-> exn? request? response?)
  (response/xexpr
     `(html
        (head
          (title "500 Internal Server Error"))
        (body
          (h1 "Internal Server Error")
          (p ,(format "An internal server error was encountered while processing your request for URL ~a." (url->string (request-uri request))))
          (code 
            ,(if (and (read-config 'debug) exn)
              (format-stack-trace (exn-continuation-marks exn))
              ""))
          (hr)
          (address ,(format "Seashell/1.0 running on Racket ~a on ~a"
                            (version) (request-host-ip request)))))
    #:code 500
    #:message #"Internal Server Error"
    #:preamble #"<!DOCTYPE HTML>"))

;; (project-export-page request?) -> response?
;; Downloads a project as a ZIP file.
(define/contract (project-export-page request)
  (-> request? response?)
  (with-handlers
    ([exn:project? (lambda (exn) (standard-error-page request))]
     [exn:misc:match? (lambda (exn) (standard-error-page request))]
     [exn:authenticate? (lambda (exn) (standard-unauthorized-page request))]
     [exn? (lambda (exn) (standard-server-error-page exn request))])
    (define bindings (request-bindings/raw request))
    (match-define (binding:form _ raw-token) (bindings-assq "token" bindings))
    (define project (check-download-token (bytes->jsexpr raw-token)))
    (response/full 200 #"Okay"
      (current-seconds)
      #"application/zip, application/octet-stream"
      empty
      (export-project project))))

(define project-export-dispatcher (lift:make project-export-page))
