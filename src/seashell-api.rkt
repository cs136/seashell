(module seashell-api racket
  (require web-server/servlet)
  (provide start-api)
  (define (start-api req)
    (response/xexpr
      `(html (body (h1 "It works!"))))))
