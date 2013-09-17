#lang racket
(require compiler/cm
         seashell/seashell-config)

(sequence-for-each
  (lambda(path)
    (when (and (file-exists? path) (equal? (filename-extension path) #"rkt"))
      (printf "Compiling ~a~n" path)
      (managed-compile-zo path)))
  (in-directory (build-path (read-config 'seashell-collects))))
