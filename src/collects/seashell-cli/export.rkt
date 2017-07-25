#lang racket

(require racket/cmdline
         seashell/backend/project)

(provide export-main)

(define (export-main flags)
  (define will-export-all (make-parameter #f))
  (define as-zip (make-parameter #f))
  (define args
    (command-line
      #:program "seashell-cli export"
      #:argv flags
      #:usage-help "Export projects from the database to the filesystem or as a zip file."
      #:once-each
      [("-a" "--all") "Export all projects"
                      (will-export-all #t)]
      [("-z" "--zip") "Export as a zip file"
                      (as-zip #t)]
      #:args args
      args))
  (cond
    [(and (will-export-all) (as-zip))
      (printf "Cannot export all projects as a zip file.")]
    [(will-export-all)
      (unless (= 1 (length args))
        (error "export -a expects one argument (target directory)"))
      (export-all (first args))]
    [else
      (unless (= 2 (length args))
        (error "export expects two arguments (project name, target directory)"))
      (export-project-name (first args) (as-zip) (second args))]))


