;; Seashell (C) 2012 Jennifer Wong, Marc Burns.
;;  University of Waterloo
;; Seashell is a server-side C code editing and
;; execution environment for web browsers.
(module seashell racket
(require web-server/managers/lru
         web-server/servlet
         web-server/servlet-env
         web-server/private/xexpr
         net/url
         "config.rkt"
         "database.rkt"
         "log.rkt"
         "format-trace.rkt"
         "common.rkt"
         "seashell-api.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Entry point for every new request.
(define (start req)
  (match (request-path-string req)
    [(regexp #rx"^/api.*$")
     (start-api req)]
    [else (error 'seashell "Undefined action: ~a" (request-path-string req))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Start the stand-alone Racket server.

;; Session expiration handler. Take users back to the application root.
(define (session-exp-handler req)
  (response/json '((status . #f))))

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

(define (seashell-servlet-error-responder url exn)
  (logf 'web-exn
        (format "Servlet (@ ~a) exception:\n~a ~a\n" (url->string url) (exn-message exn)
                (foldl string-append ""
                  (format-stack-trace
                   (continuation-mark-set->context
                    (exn-continuation-marks exn))))))
  (seashell-pretty-exception-response url exn))

(define server-root-path ".")

(define (start-seashell-webserver hostname port)
  (thread
   (thunk
    ;; Continuation manager. Timeout seems to have a subtle memory-corruption
    ;; bug, so we'll use LRU.
    (let ((manager (make-threshold-LRU-manager session-exp-handler (* 768 1024 1024))))
      (serve/servlet start
                     #:command-line?      #t
                     #:listen-ip          hostname
                     #:port               port
                     #:manager            manager
                     #:servlet-regexp     #rx"^/api/.*$"
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

(logf 'info "Seashell stopped.")
(sleep 0.5) ;; Cleanup time.
(exit 0))

