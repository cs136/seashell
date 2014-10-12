#lang racket
(require setup/parallel-build
         seashell/seashell-config)

(parallel-compile-files
  (filter
    (lambda (path)
      (and (file-exists? path)
           (equal? (filename-extension path) #"rkt")))
    (sequence->list (in-directory (build-path (read-config 'seashell-collects)))))
  #:handler (lambda (_ type work msg out err)
    (match type
      ['start (void)]
      ['done (printf " Made ~a\n" work)]
      ['output (printf " Output from: ~a\n~a~a" work out err)]
      [else (printf " Error compiling ~a\n~a\n~a~a"
                    work
                    msg
                    out
                    err)
            (exit 1)])))
