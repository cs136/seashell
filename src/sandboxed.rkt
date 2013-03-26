(module sandboxed racket
  (require web-server/managers/lru
           web-server/servlet
           web-server/servlet-env
           web-server/private/xexpr)
  (define (start req)
    (response/xexpr '(html (body "It worked!"))))
  (define th
    (thread
     (thunk
      ;; Continuation manager. Timeout seems to have a subtle memory-corruption
      ;; bug, so we'll use LRU.
      (let ((manager (make-threshold-LRU-manager #f (* 768 1024 1024))))
        (serve/servlet start
                       #:command-line?      #t
                       #:listen-ip          "129.97.134.17"
                       #:port               8888
                       #:manager            manager
                       #:servlet-regexp     #rx"^/api/.*$"
                       #:server-root-path   "/users/m4burns/seashell/src"
                       #:servlet-responder  (lambda(a b) a)
                       #:connection-close?  #t
                       #:log-file           (build-path "/users/m4burns/seashell/src/seashell-access.log"))))))
  (thread-wait th))
