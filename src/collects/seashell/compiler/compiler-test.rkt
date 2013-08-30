#lang racket
(require "compiler.rkt")

(define cc (make-seashell-compiler))

(define include-search-path
  '("-I/usr/local/include"
    "-I/usr/lib/gcc/x86_64-linux-gnu/4.7.2/include"
    "-I/usr/x86_64-linux-gnu/include"
    "-I/usr/include"))

(define-values (bin diag)
               (seashell-compile-files cc
                                       (append include-search-path '("-Wall"))
                                       '()
                                       `(,(string->path "foo.c") ,(string->path "bar.c"))))

(pretty-print diag)
(with-output-to-file "foo" (thunk (write-bytes bin)) #:exists 'replace)
