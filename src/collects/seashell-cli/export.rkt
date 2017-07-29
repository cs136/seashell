#lang typed/racket

(require racket/cmdline
         (submod seashell/seashell-config typed)
         seashell/db/database
         seashell/backend/project)

(provide export-main)

(: export-main (-> (Listof String) Void))
(define (export-main flags)
  (: will-export-all (Parameter Boolean))
  (define will-export-all (make-parameter #f))
  (: as-zip (Parameter Boolean))
  (define as-zip (make-parameter #f))
  (: selected-user (Parameter (U False String)))
  (define selected-user (make-parameter #f))
  (: args (Listof String))
  (define args
    (parse-command-line "seashell-cli export" flags
      `((once-each
        [("-u" "--user")
          ,(lambda (flag user) (selected-user (cast user String)))
          ("Access another user's database (works only if you have permissions on the linux environment)"
           "Username")]
        [("-a" "--all")
          ,(lambda (flag) (will-export-all #t))
          ("Export all projects")]
        [("-z" "--zip")
          ,(lambda (flag) (as-zip #t))
          ("Export as a zip file")]))
      (lambda (flag-accum . args) (cast args (Listof String)))
      '("args")))
  (init-sync-database (if (selected-user)
    (format (read-config-string 'database-file-format) (selected-user))
    #f))
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


