#lang racket

(require rackunit
         seashell/backend/project
         seashell/backend/runner
         seashell/backend/files
         seashell/compiler/place
         seashell/seashell-config)

(define (create-project-with-contents-and-run contents)
  (when (is-project? "foo")
    (delete-project "foo"))
  (new-project "foo")
  (make-directory (check-and-build-path (build-project-path "foo") "q1"))
  (with-output-to-file (check-and-build-path (build-project-path "foo") "q1" "test.c")
    (thunk (display contents)))
  (compile-and-run-project "foo" "q1/test.c" "q1" '()))


(define/provide-test-suite compiler-suite
  (test-suite "Compiler Tests"

    (test-case "ok-print-Hello."
      (define-values (success hsh) (create-project-with-contents-and-run 
        "#include <stdio.h>\nint main() {\nprintf(\"Hello.\");\n}\n"))
      (check-true success)
      (define pid (hash-ref hsh 'pid))
      (sync (program-wait-evt pid))
      (check string=? (port->string (program-stdout pid)) "Hello."))

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

;; Put this test back when clang is upgraded
;    (test-case "clang-hanger"
;      (define-values (res hsh) (create-project-with-contents-and-run
;#<<EOF
;struct MyStruct { int my_member; };
;void f(struct MyStruct *a_struct) {
;    non_existant_function(*astruct->y_member.junk, *astruct.my_);
;}
;int main() {}
;EOF
;        ))
;      (check-false res))

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

    (delete-project "foo")
    ))
