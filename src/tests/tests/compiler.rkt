#lang racket

(require rackunit
         seashell/backend/project
         seashell/backend/runner
         seashell/backend/files
         seashell/compiler/place
         seashell/seashell-config)

(define (create-project-with-contents-and-run contents)
  (define tmpdir #f)
  (dynamic-wind
    (thunk (set! tmpdir (make-temporary-file "seashell-test-proejct-~a" 'directory)))
    (thunk
      (make-directory (build-path tmpdir "q1"))
      (with-output-to-file (build-path tmpdir "q1" "test.c")
        (thunk (write-string contents)))
      (define-values (succ hsh) (compile-and-run-project (path->string tmpdir) "q1/test.c" "q1" '()))
      (when succ (sync (program-wait-evt (hash-ref hsh 'pid))))
      (values succ hsh))
    (thunk (delete-directory/files tmpdir))))

(define/provide-test-suite compiler-suite
  (test-suite "Compiler Tests"

    (test-case "ok-print-Hello."
      (define-values (success hsh) (create-project-with-contents-and-run 
        "#include <stdio.h>\nint main() {\nprintf(\"Hello.\\n\");\n}\n"))
      (check-true success)
      (define pid (hash-ref hsh 'pid))
      (sync (program-wait-evt pid))
      (eprintf ">>> ~a~n" (port->string (program-stderr pid)))
      (check string=? (port->string (program-stdout pid)) "Hello.\r\n"))

    (test-case "ok-shutdown-compiler"
      (seashell-compile-place/shutdown)
      (check-false (seashell-compile-place/alive?)))

    (test-case "fail-invalid-C"
      (define-values (res hsh) (create-project-with-contents-and-run "great code;"))
      (check-false res)
      (check string=? (hash-ref hsh 'status) "compile-failed"))

    (test-case "bad-werror-return-type-no-return"
      (define-values (res hsh) (create-project-with-contents-and-run
#<<EOF
  int a() {
  }
  int main() {
  }
EOF
        ))
      (check-false res))
    
    (test-case "bad-werror-return-type-return-void"
      (define-values (res hsh) (create-project-with-contents-and-run
#<<EOF
  void a() {
    return 5;
  }
  int main() {
  }
EOF
        ))
      (check-false res))
    
    (test-case "bad-werror-int-conversion"
      (define-values (res hsh) (create-project-with-contents-and-run
#<<EOF
  int main() {
    int a = &a;
  }
EOF
        ))
      (check-false res))
    
    
    (test-case "bad-werror-int-to-pointer-cast"
      (define-values (res hsh) (create-project-with-contents-and-run
#<<EOF
int x(long *f) {
}

int main() {
  int i = 0;
    x((void*) i);
    }
EOF
        ))
      (check-false res))
    ))
