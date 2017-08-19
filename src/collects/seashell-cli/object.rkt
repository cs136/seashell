#lang typed/racket

(require seashell/compiler
         racket/cmdline)

(provide object-main)

(: object-main (-> (Listof String) Void))
(define (object-main flags)
  (define args
    (parse-command-line "seashell-cli object" flags
      '()
      (lambda (flag-accum . args) (cast args (Listof String)))
      '("Generate object files (actually LLVM bytecode) for use with Seashell. Takes in a list of files to generate code for.")))
  (void (map (lambda ([arg : String])
    (define-values (result errs) (seashell-generate-bytecode (path->complete-path (string->path arg))))
    (if result
      (let ([fparts (string-split arg ".")])
        (with-output-to-file (string-join (append (drop-right fparts 1) '("ll")) ".")
          (thunk (display result)) #:exists 'replace))
      (write errs))) args)))


