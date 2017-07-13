#lang racket

(require seashell-cli/marmtest
         seashell-cli/object
         seashell-cli/export
         seashell/log)

(define tools `#hash(("marmtest" . (,marmtest-main "Marmoset test runner."))
                     ("object" . (,object-main "Generate object files for Seashell."))
                     ("export" . (,export-main "Export projects from a user's Seashell database."))))

;; main program execution begins here
(define flags (vector->list (current-command-line-arguments)))

(when (or (empty? flags) (not (hash-has-key? tools (first flags))))
  (printf "seashell-cli <tool>; possible tools are:~n")
  (hash-for-each tools (lambda (tool props)
    (printf "  ~a: ~a~n" tool (second props))))
  (exit 1))

(standard-logger-setup)
;; invoke main method for tool
((first (hash-ref tools (first flags))) (rest flags))
