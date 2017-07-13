#lang racket

(require seashell/compiler
         racket/cmdline)

(provide object-main)

(define (object-main flags)
  (define args
    (command-line
      #:program "seashell-cli object"
      #:argv flags
      #:usage-help "Generate object files (actually LLVM bytecode) for use with Seashell. Takes in a list of files to generate code for."
      #:args args
      args))
  (void (map (lambda (arg)
    (define-values (result errs) (seashell-generate-bytecode (path->complete-path (string->path arg))))
    (if result
      (let ([fparts (string-split arg ".")])
        (with-output-to-file (string-join (append (drop-right fparts 1) '("ll")) ".")
          (thunk (display result)) #:exists 'replace))
      (write errs))) args)))


