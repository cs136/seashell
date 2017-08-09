#lang typed/racket

(require racket/cmdline
         (submod seashell/seashell-config typed)
         seashell/db/database
         seashell/backend/project)

(provide import-main)

(: import-main (-> (Listof String) Void))
(define (import-main flags)
  (define-values (proc out in err) (subprocess #f #f #f (read-config-string 'whoami)))
  (subprocess-wait proc)
  (when (not (zero? (cast (subprocess-status proc) Integer)))
    (raise (exn:fail "Could not identify current user." (current-continuation-marks))))
  (: selected-user (Parameter String))
  (define selected-user (make-parameter (port->string out)))
  (: args (Listof String))
  (define args
    (parse-command-line "seashell-cli import" flags
      `((once-each
        [("-u" "--user")
          ,(lambda (flag user) (selected-user (cast user String)))
          ("Access another user's database (works only if you have permissions on the linux environment)"
           "Username")]))
      (lambda (flag-accum . args) (cast args (Listof String)))
      '("args")))
  (init-sync-database (if (selected-user)
    (format (read-config-string 'database-file-format) (selected-user))
    #f))
  (unless (= 2 (length args))
    (error "import expects two arguments (zip file name, project name)"))
  (new-project (second args) (first args))
  (void))


