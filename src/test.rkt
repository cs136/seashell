(module test racket
  (require web-server/managers/lru
           web-server/servlet
           web-server/servlet-env
           web-server/private/xexpr
           racket/sandbox
           setup/dirs)

  (define allowable-modules
    '(racket
       web-server/managers/lru
       web-server/servlet
       web-server/servlet-env
       web-server/private/xexpr))

  (define whitelist-read-dirs
    `(,(string->path "/users/m4burns/seashell/src/")
      ,(path->complete-path
        (find-system-path 'collects-dir)
        (find-executable-path (find-system-path 'exec-file)))
      ,(find-system-path 'pref-dir)
      ,(find-lib-dir)))

  (define (make-my-security-guard)
    (define parent-guard (current-security-guard))
    (define (realpath p)
      (if (or (file-exists? p) (directory-exists? p))
          (normalize-path p (current-directory))
          (simplify-path p #f)))
    (define (directory-parents-entity? d e)
      (let* ((nd (realpath d))
             (ne (realpath e))
             (ds (path->string nd))
             (es (path->string ne)))
        (and (directory-exists? nd)
             (absolute-path? ne)
             (>= (string-length es) (string-length ds))
             (string=? ds (substring es 0 (string-length ds))))))
    (define (read-op? o)
      (andmap (lambda(o) (memq o '(read exists))) o))
    (define (white-path? p)
      (ormap
       ((curryr directory-parents-entity?) p)
       whitelist-read-dirs))
    (make-security-guard
      parent-guard
      (lambda(caller path op)
        (parameterize ([current-security-guard parent-guard])
         (if (and path (read-op? op))
             (match
               (realpath path)
               [(? white-path?) #t]
               [else (error 'sandbox-filesystem "Access denied to file ~a by caller ~a for operation ~a." path caller op)])
             (match
               op
               ['(exists) #t]
               [else (error 'sandbox-filesystem "Access denied for operation ~a by caller ~a." op caller)]))))
      (lambda(caller host port end)
        (match
          `(,caller ,host ,port ,end)
          [`(,_ "129.97.134.17" 8888 server) #t]
          [else
           (error 'sandbox-network "Access denied for network: ~a ~a ~a ~a." caller host port end)]))
      #f))

  (parameterize
    ([sandbox-security-guard (make-my-security-guard)]
     [sandbox-memory-limit 30]
     [sandbox-eval-handlers `(,(lambda(t) (call-with-custodian-shutdown t))
                              ,(lambda(t) (call-with-custodian-shutdown t)))])
      (make-module-evaluator (file->string "sandboxed.rkt")
                             #:allow-for-require allowable-modules)))
