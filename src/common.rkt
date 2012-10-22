;; Routines which are commonly useful.
(module seashell-common racket
  (require web-server/servlet
           json)
  (provide embed-jquery
           request-path-string
           response/json
           make-api-error)

  ;; Embed jQuery in a page.
  (define embed-jquery
    `(script [(type "text/javascript")
              (src  "https://ajax.googleapis.com/ajax/libs/jquery/1.8.2/jquery.min.js")]))

  ;; Convert list of pairs to a JSON response.
  (define (response/json assocs)
    (response/full 200 #"Good" (current-seconds)
                   TEXT/HTML-MIME-TYPE
                   empty
                   (list (jsexpr->bytes (make-hash assocs)))))

  ;; Make a JSON response indicating an API error.
  (define (make-api-error code str)
    (response/json
      `((error . ,(make-hash `((code . ,code) (reason . ,str)))))))

  ;; Convert request to path string.
  (define (request-path-string req)
    (foldl
     (lambda(c b)
       (string-append b "/" (path/param-path c))) ""
                                                  (url-path (request-uri req)))))
