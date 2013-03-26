(module test racket
  (require web-server/managers/lru
           web-server/servlet
           web-server/servlet-env
           web-server/private/xexpr
           racket/sandbox)

  (define allowable-modules
    '(racket
       web-server/managers/lru
       web-server/servlet
       web-server/servlet-env
       web-server/private/xexpr))

  (define (make-my-security-guard)
    (define (realpath p)
      (normalize-path p (current-directory)))
    (define whitelist-read-dirs
      `(,(string->path "/users/m4burns/seashell/src/")
        ,(path->complete-path
          (find-system-path 'collects-dir)
          (find-executable-path (find-system-path 'exec-file)))
        ,(find-system-path 'pref-dir)))
    (define parent-guard (current-security-guard))
    (define pf (realpath (find-system-path 'pref-file)))
    (define (directory-parents-entity? d e)
      (let* ((nd (realpath d))
             (ne (realpath e))
             (ds (path->string nd))
             (es (path->string ne)))
        (and (directory-exists? nd)
             (or (file-exists? ne) (directory-exists? ne))
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
         (printf "Called security guard: ~a ~a ~a~n" caller path op)
         (if (and path (read-op? op))
             (match
               (realpath path)
               [(? ((curry equal?) pf)) #t]
               [(? white-path?) #t]
               [else (error 'sandbox-filesystem "Access denied to file ~a by caller ~a for operation ~a." path caller op)])
             (match
               op
               ['(exists) #t]
               [else (error 'sandbox-filesystem "Access denied for operation ~a by caller ~a." op caller)]))))
      (lambda(caller host port end)
        (printf "Called network guard: ~a ~a ~a ~a~n" caller host port end)
        (error 'sandbox-network "Access denied for network operation."))
      #f))

  (parameterize
    ([current-security-guard (make-my-security-guard)])
      (make-module-evaluator (file->string "sandboxed.rkt")
                             #:allow-for-require allowable-modules)))
