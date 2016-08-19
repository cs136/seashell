#lang typed/racket
(require typed/racket/date)
(provide prepare-compile-file make-script-section make-style-section)

(: read-resource-file (-> Path-String (Listof String)))
(define (read-resource-file path)
  (with-input-from-file path
    (thunk
      (for/list : (Listof String)
        ([line (in-lines)]
         #:unless (regexp-match "^$|^#" line))
        line))))

(: local-resource? (-> String Boolean))
(define (local-resource? resource)
  (not (regexp-match "^//" resource)))

(: compile-js-resources (->* ((Listof String)) (Boolean String (Option String)) (Listof String)))
(define (compile-js-resources resources [minify? #f] [target ""] [uglifyjs-path #f])
  (define local-resources (filter local-resource? resources))
  (define other-resources (filter (compose not local-resource?) resources))
  (cond
    [(not minify?) resources]
    [(not uglifyjs-path) resources]
    [else
      (define target-js (string-append target ".min.js"))
      (define target-js-map (string-append target ".js.map"))
      (apply system* (list* uglifyjs-path
                            "-o" target-js
                            "--source-map" target-js-map
                            "--compress"
                            "--mangle"
                            "--keep-fnames"
                            "--"
                            local-resources))
      (list* target-js other-resources)]))

(: compile-css-resources (->* ((Listof String)) (Boolean String (Option String)) (Listof String)))
(define (compile-css-resources resources [minify? #f] [target ""] [uglifycss-path #f])
  (define local-resources (filter local-resource? resources))
  (define other-resources (filter (compose not local-resource?) resources))
  (cond
    [(not minify?) resources]
    [(not uglifycss-path) resources]
    [else
      (define target-css (string-append target ".min.css"))
      (with-output-to-file target-css #:exists 'truncate
        (thunk
          (apply system* (list* uglifycss-path
                                local-resources))))
      (list* target-css other-resources)]))

(: make-manifest-file (-> Path-String (Listof String) Any))
(define (make-manifest-file target resources)
  (with-output-to-file target #:exists 'truncate
    (thunk
      (printf "CACHE MANIFEST~n")
      (printf "# Generated on: ~a~n" (date->string (current-date) #t))
      (for ([r resources])
        (printf "~a~n" r))
      (printf "NETWORK:~n")
      (printf "*~n"))))

(: prepare-compile-file (->* (String Path-String Path-String Path-String)
                             (Boolean (Option String) (Option String))
                             (Values (Listof String) (Listof String))))
(define (prepare-compile-file target js-resource-file css-resource-file extra-resource-file
                              [minify? #f] [uglifyjs-path #f] [uglifycss-path #f])
  (define js-resources (read-resource-file js-resource-file))
  (define css-resources (read-resource-file css-resource-file))
  (define extra-resources (read-resource-file extra-resource-file))
  (define manifest-target (string-append target ".manifest"))
  (cond
    [(not minify?)
      (make-manifest-file manifest-target (append js-resources (append css-resources (append extra-resources))))
     (values js-resources css-resources)]
    [else
      (define minified-js-resources (compile-js-resources js-resources #t target uglifyjs-path))
      (define minified-css-resources (compile-css-resources css-resources #t target uglifycss-path))
      (make-manifest-file manifest-target (append minified-js-resources (append minified-css-resources (append extra-resources))))
      (values minified-js-resources minified-css-resources)]))

(: make-script-section (-> (Listof String) String))
(define (make-script-section jsfiles)
  (with-output-to-string
    (thunk
      (for ([jsfile jsfiles])
        (printf "<script src=\"~a\"></script>~n" jsfile)))))

(: make-style-section (-> (Listof String) String))
(define (make-style-section cssfiles)
  (with-output-to-string
    (thunk
      (for ([cssfile cssfiles])
        (printf "<link rel=\"stylesheet\" href=\"~a\"></script>~n" cssfile)))))
