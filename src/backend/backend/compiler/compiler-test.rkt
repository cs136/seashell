#lang racket
(require "compiler.rkt")

(define cc (make-seashell-compiler))

(define-values (bin diag)
               (seashell-compile-files cc '("-Wall") '() `(,(string->path "foo.c"))))

(pretty-print diag)
(when bin (pretty-print (vector-length bin)))
