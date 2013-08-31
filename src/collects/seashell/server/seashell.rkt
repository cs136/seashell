#lang racket
;; Seashell's gateway server.
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
(module seashell-gateway racket
  (require web-server/managers/lru
           web-server/servlet
           web-server/servlet-env
           web-server/private/xexpr
           net/url
           seashell/seashell-config
           seashell/log
           seashell/format-trace
           "common.rkt" ;; TODO
           "seashell-api.rkt") ;; TODO

  ;; Entry point for every new request.
  (define (start req)
    (printf "~a~n" (request-path-string req))
    (match (request-path-string req)
      [(regexp #rx"^/api.*$")
       (start-api req)]
      [else (redirect-to server-root-url temporarily)])) ;; TODO

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Start the stand-alone Racket server.

  ;; Session expiration handler. Take users back to the application root.
  (define (session-exp-handler req)
    (redirect-to server-root-url temporarily))

  ;; TODO silence stack trace when not in debug mode
  (define (seashell-pretty-exception-response url exn)
    (response/xexpr
     #:code 500
     `(html
       (head
        (title "Server Error")
        (link ([rel "stylesheet"] [href ,(string-append res-root "error.css")])))
       (body
        (div ([class "section"])
             (div ([class "title"]) "Exception")
             (p
              "The application raised an exception with the message:"
              (pre ,(if (exn:pretty? exn)
                        (exn:pretty-xexpr exn)
                        (exn-message exn))))
             (p
              "Stack trace:"
              (pre
               ,@(format-stack-trace
                  (continuation-mark-set->context (exn-continuation-marks exn))))))))))

  ;; TODO
  (define (seashell-servlet-error-responder url exn)
    (logf 'web-exn
          (format "Servlet (@ ~a) exception:\n~a ~a\n" (url->string url) (exn-message exn)
                  (foldl string-append ""
                         (format-stack-trace
                          (continuation-mark-set->context
                           (exn-continuation-marks exn))))))
    (seashell-pretty-exception-response url exn))

  (define (start-seashell-webserver hostname port)
    (thread
     (thunk
      (let ((manager (make-threshold-LRU-manager session-exp-handler (* 768 1024 1024)))) ;; TODO configurable
        (serve/servlet start ;; TODOTODOTODO
                       #:command-line?      #t
                       #:listen-ip          hostname
                       #:port               port
                       #:manager            manager
                       #:servlet-regexp     #rx"^.*$"
                       #:server-root-path   server-root-path
                       #:servlet-responder  seashell-servlet-error-responder
                       #:connection-close?  #t
                       #:log-file           (build-path server-root-path "seashell-access.log"))))))

  ;; Monolithic Seashell process.
  (printf "Starting Seashell...~n")

  (define (setup-log regexp file)
    (printf "Logging messages matching '~a' to '~a'.~n" regexp file)
    (make-fs-logger regexp file)
    (void))

  (setup-log "^web-exn$" "seashell-error.log")
  (setup-log "^info$" "info.log")
  (setup-log "^warn$" "warn.log")
  (setup-log "^exception$" "exn.log")

  (define ss-exn-handler
    (lambda(e)
      (when (not (exn:break? e))
        (logf 'exception "~a:~n ~a"
              (exn-message e)
              (foldl string-append ""
                     (format-stack-trace
                      (continuation-mark-set->context
                       (exn-continuation-marks e))))))
      ((error-escape-handler))))

  (define listen-exns (make-log-reader "^exception$"))

  (uncaught-exception-handler ss-exn-handler)

  (define web-server-thread
    (thread (thunk (start-seashell-webserver
                    seashell-webserver-host
                    seashell-webserver-port))))

  (define db-thread
    (thread (thunk (start-database-server
                    seashell-db-file
                    seashell-db-host
                    seashell-db-port))))

  (logf 'info "Seashell started.")

  (parameterize-break #t
                      (with-handlers ([exn:break? (lambda(e) (logf 'info "Got SIGINT."))])
                        (let loop () (display (listen-exns)) (loop))))

  (kill-thread web-server-thread)
  (kill-thread db-thread)

  (logf 'info "Seashell stopped.")
  (exit 0))

