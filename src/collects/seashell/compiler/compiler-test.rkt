#lang racket
(require seashell/compiler)

(define-values (bin diag)
               (seashell-compile-files/place
                                       '("-Wall")
                                       '()
                                       `(,(string->path "foo.c") ,(string->path "bar.c"))))

(pretty-print diag)
(when bin (with-output-to-file "foo" (thunk (write-bytes bin)) #:exists 'replace))
