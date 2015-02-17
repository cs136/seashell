#lang racket
;; Seashell's backend server.
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
;a;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
(require
  json
  net/url
  seashell/log
  seashell/backend/authenticate
  seashell/backend/project
  seashell/backend/files
  seashell/seashell-config
  web-server/http/xexpr
  web-server/http/response-structs
  web-server/http/request-structs
  web-server/dispatchers/dispatch
  web-server/private/connection-manager
  (prefix-in lift: web-server/dispatchers/dispatch-lift)
  (prefix-in log: web-server/dispatchers/dispatch-log))

(provide request-logging-dispatcher
         standard-error-dispatcher
         project-export-dispatcher
         upload-file-dispatcher)

;; Default headers.
(define default-headers
  `(,(make-header #"Server" (string->bytes/utf-8 (format "Seashell/~a" SEASHELL_VERSION)))
    ,(make-header #"Access-Control-Allow-Origin" #"*")))

;; Default footer.
(define (make-default-footer request)
  `((hr)
    (address ,(format "Seashell/~a running on Racket ~a on ~a"
                      SEASHELL_VERSION (version) (request-host-ip request)))))
 

;; (make-headers name value ...)
;; Creates the HTTP response headers.
;;
;; Arguments:
;;  headers -> List of pairs of name/value bytes.
;; Returns:
;;  List of HTTP headers.
(define make-headers
  (case-lambda
    [(name value . rest)
     (cons (make-header name value) (apply make-headers rest))]
    [()
     default-headers]))

;; (request-logging-dispatcher) -> (void?)
;; Dispatcher that logs all incoming requests to the standard log.
(define/contract (request-logging-dispatcher connection request)
  (-> connection? request? void?)
  (logf 'info (string-trim (log:apache-default-format request)))
  (next-dispatcher))

;; (standard-empty-response request?) -> response?
;; Sends the standard Seashell empty response.
(define/contract (standard-empty-response request)
  (-> request? response?)
  (response/xexpr
     `(html
        (head
          (title "Nothing here."))
        (body
          (h1 "So down")
          ,@(make-default-footer request)))
    #:code 200
    #:headers (make-headers)
    #:message #"OK"
    #:preamble #"<!DOCTYPE HTML>"))

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
          ,@(make-default-footer request)))
    #:code 404
    #:headers (make-headers)
    #:message #"Not Found"
    #:preamble #"<!DOCTYPE HTML>"))
(define standard-error-dispatcher (lift:make standard-error-page))

;; (standard-unauthenticated-page exn request?) -> response?
;; Sends the standard Seashell unauthenticated page.
(define/contract (standard-unauthorized-page exn request)
  (-> exn? request? response?)
  (response/xexpr
     `(html
        (head
          (title "403 Forbidden"))
        (body
          (h1 "Forbidden")
          (p  ,(format "You are not authorized to view the request URL ~a.
                        The reason provided was: ~a" (url->string (request-uri request))
                        (exn-message exn)))
          ,@(make-default-footer request)))
    #:code 403
    #:message #"Forbidden"
    #:headers (make-headers)
    #:preamble #"<!DOCTYPE HTML>"))

;; (standard-server-error-page exn request?) -> response?
;; Sends the standard Seashell server error page.
(define/contract (standard-server-error-page exn request)
  (-> exn? request? response?)
  (logf 'error "Error in handling ~a ~a: ~a." (request-method request)
        (url->string (request-uri request)) (exn-message exn))
  (response/xexpr
     `(html
        (head
          (title "500 Internal Server Error"))
        (body
          (h1 "Internal Server Error")
          (p ,(format "An internal server error was encountered while processing your request for URL ~a." (url->string (request-uri request))))
          ,@(if (and (read-config 'debug) exn)
              `((hr)
                (pre
                  ,(format "Message: ~a\n" (exn-message exn))
                  ,(format-stack-trace (exn-continuation-marks exn))))
              '(""))
          ,@(make-default-footer request)))
    #:code 500
    #:message #"Internal Server Error"
    #:headers (make-headers)
    #:preamble #"<!DOCTYPE HTML>"))

;; (project-export-page request?) -> response?
;; Downloads a project as a ZIP file.
(define/contract (project-export-page request)
  (-> request? response?)
  (with-handlers
    ([exn:project? (lambda (exn) (standard-error-page request))]
     [exn:misc:match? (lambda (exn) (standard-server-error-page exn request))]
     [exn:fail:contract? (lambda (exn) (standard-server-error-page exn request))]
     [exn:authenticate? (lambda (exn) (standard-unauthorized-page exn request))]
     [exn? (lambda (exn) (standard-server-error-page exn request))])
    (define bindings (request-bindings/raw request))
    (match-define (binding:form _ raw-token) (bindings-assq #"token" bindings))
    (define project (check-download-token (bytes->jsexpr raw-token)))
    (response/full 200 #"OK"
      (current-seconds)
      #"application/zip"
      (make-headers
        #"Content-Disposition"
        #"attachment")
      `(,(export-project project)))))
(define project-export-dispatcher (lift:make project-export-page))

;; (upload-file-page request?) -> response?
;; Uploads a file from the user to a project
(define/contract (upload-file-page request)
  (-> request? response?)
  (with-handlers
    ([exn:project? (lambda (exn) (standard-error-page request))]
     [exn:misc:match? (lambda (exn) (standard-server-error-page exn request))]
     [exn:fail:contract? (lambda (exn) (standard-server-error-page exn request))]
     [exn:authenticate? (lambda (exn) (standard-unauthorized-page exn request))]
     [exn? (lambda (exn) (standard-server-error-page exn request))])
    (define bindings (request-bindings/raw request))
    (match-define (binding:form _ raw-token) (bindings-assq #"token" bindings))
    (match-define (binding:file _ _ _ content) (bindings-assq #"file-to-upload" bindings))
    (match-define (list project filename) (check-upload-token (bytes->jsexpr raw-token)))
    (new-file project filename content 'raw)
    (standard-empty-response request)))
(define upload-file-dispatcher (lift:make upload-file-page))
