(module seashell-api racket
  (require web-server/servlet
           web-server/servlet-env
           "common.rkt"
           "database.rkt")
  (provide start-api)

  (define (do-save req)
    ;; save the document here.
    (printf "do-save~n")
    (do-save
     (send/suspend
      (lambda(url)
        (response/json
          `((k . ,(make-hash `((save . ,url))))))))))

  (define (do-open req)
    (define (do-open-r req n)
      ;; open the document here.
      (printf "do-open ~a~n" n)
      (do-open-r
       (send/suspend
         (lambda(url)
           (response/json
             `((k . ,(make-hash `((open . ,url))))))))
       (+ n 1)))
    (do-open-r req 0))

  (define (do-run req)
    (printf "do-run~n")
    (send/suspend
      (lambda(url)
        (response/json
          `((run . ,url))))))

  ;; Start a new document.
  (define (new-handler req)
    (send/suspend/dispatch
      (lambda(embed/url)
        (response/json
          `((save . ,(embed/url do-save))
            (open . ,(embed/url do-open))
            (run  . ,(embed/url do-run)))))))

  (define (start-api req)
    (match (request-path-string req)
      [(regexp #rx"^/api/new.*$")
       (new-handler req)])))
