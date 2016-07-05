#lang racket

(require rackunit
         racket/serialize
         seashell/backend/project
         seashell/backend/runner
         seashell/seashell-config)

(define test-add-hdr "int add(int, int);\n")
(define test-add-imp "#include \"add.h\"\nint add(int a, int b){ return a+b; }\n")
(define test-mult-hdr "int mult(int, int);\n")
(define test-mult-imp "#include \"multiply.h\"\n#include \"mod2.h\"\nint mult(int a, int b){ noop(); return a*b; }\n")
(define test-mod2-hdr "void noop(void);\n")
(define test-mod2-imp "#include \"mod2.h\"\nvoid noop(void){}\n")
(define test-main-file #<<HERE
#include <stdio.h>
#include "add.h"
#include "multiply.h"
int main(){
    int a,b;
    scanf("%d", &a);
    scanf("%d", &b);
    if(a == 0) return 1;
    printf("%d\n", add(a,mult(1,b)));
}
HERE
)

(define/provide-test-suite project-suite
  (test-suite "Project Tests"
    (test-case "Create a Project"
      (new-project "foo")
      (check-pred is-project? "foo"))

    (test-case "Lock a project"
      (sleep 2) ;; make sure we have enough delay to make sure timestamps
                ;; will be updated correctly.
      (define current-timestamp (file-or-directory-modify-seconds
                                  (build-project-path "foo")))
      (check-true (lock-project "foo" (current-thread)))
      (define new-timestamp (file-or-directory-modify-seconds
                             (build-project-path "foo")))
      (check < current-timestamp new-timestamp))

    (test-case "Lock a locked project"
      (sleep 2) ;; make sure we have enough delay to make sure timestamps
                ;; will be updated correctly.
      (define current-timestamp (file-or-directory-modify-seconds
                                  (build-project-path "foo")))
      (sync (thread (thunk
        (check-false (lock-project "foo" (current-thread))))))
      (define new-timestamp (file-or-directory-modify-seconds
                             (build-project-path "foo")))
      (check-equal? current-timestamp new-timestamp))

    (test-case "Force lock a locked project"
      (sleep 2) ;; make sure we have enough delay to make sure timestamps
                ;; will be updated correctly.
      (define current-timestamp (file-or-directory-modify-seconds
                                  (build-project-path "foo")))
      (sync (thread (thunk
        (force-lock-project "foo" (current-thread)))))
      (define new-timestamp (file-or-directory-modify-seconds
                             (build-project-path "foo")))
      (check < current-timestamp new-timestamp))

    (test-case "Create an existing project."
      (check-exn exn:fail? (thunk (new-project "foo"))))
    (test-case "Delete a non-Project"
      (check-exn exn:fail? (thunk (delete-project "bar"))))

    (test-case "Run a Project"
      (with-output-to-file (check-and-build-path (build-project-path "foo") "test.c")
        (thunk (display "#include <stdio.h>\nint main() {\nprintf(\"Hello.\");\n}\n")))
      (define-values (success hsh) (compile-and-run-project "foo" "test.c" '()))
      (check-true success)
      (sync (program-wait-evt (hash-ref hsh 'pid))))

    (test-case "Run a Project with common and tests"
      (make-directory (check-and-build-path (build-project-path "foo") "q1"))
      (make-directory (check-and-build-path (build-project-path "foo") "q1" "tests"))
      (make-directory (check-and-build-path (build-project-path "foo") "common"))
      (for ([file '("q1/main.c"
                    "q1/add.h" "q1/add.c"
                    "common/multiply.h" "common/multiply.c"
                    "common/mod2.h" "common/mod2.c"
                    "q1/tests/pass.in" "q1/tests/pass.expect"
                    "q1/tests/fail.in" "q1/tests/fail.expect"
                    "q1/tests/crash.in" "q1/tests/crash.expect")]
            [contents (list test-main-file
                            test-add-hdr test-add-imp
                            test-mult-hdr test-mult-imp
                            test-mod2-hdr test-mod2-imp
                            "3\n4\n" "7\n"
                            "4\n5\n" "2\n"
                            "0\n0\n" "0\n")])
        (with-output-to-file (check-and-build-path (build-project-path "foo") file)
          (thunk (display contents))))
      (define-values (success hsh) (compile-and-run-project "foo" "q1/main.c" '("pass" "fail" "crash") #f))
      (check-true success)
      (for ([pid (hash-ref hsh 'pids)]
            [exp-result '("passed")])
        (sync (program-wait-evt pid))
        (check-equal? exp-result (third (deserialize (read (program-stdout pid)))))))

    (test-case "Get a Compilation Error"
      (with-output-to-file (check-and-build-path (build-project-path "foo") "error.c")
        (thunk (display "great code;")))
      (define-values (res hsh) (compile-and-run-project "foo" "error.c" '()))
      (check-false res)
      (check string=? (hash-ref hsh 'status) "compile-failed"))

    (test-case "Export a Project"
      (export-project "foo"))

    (test-case "Most Recently Used"
      (update-most-recently-used "foo" #f '("fexists" "error.c") "some data")
      (check string=? (get-most-recently-used "foo" #f) "some data"))

    (test-case "Delete a Project"
      (delete-project "foo")
      (check-true (not (member "foo" (list-projects)))))

    (test-case "Archive Projects"
      (new-project "bar")
      (new-project "foobar")
      (archive-projects "my-archive")
      (check-true (directory-exists? (build-path (read-config 'seashell) "archives" "my-archive")))
      (check-true (directory-exists? (build-path (read-config 'seashell) "archives" "my-archive" "bar")))
      (check-true (directory-exists? (build-path (read-config 'seashell) "archives" "my-archive" "foobar"))))

    (test-case "Read from nonexistent project settings"
      (new-project "test-project")
      (check-false (read-project-settings "test-project")))

    (test-case "update nonexistent project settings"
      (new-project "test-project-2")
      (write-project-settings/key "test-project-2" 'key "val")
      (check-equal? (read-project-settings "test-project-2") #hasheq((key . "val"))))

    (test-case "Write project settings"
      (write-project-settings "test-project" #hasheq((A . 5) (B . 6)))
      (check-equal? (read-project-settings "test-project") #hasheq((A . 5) (B . 6))))

    (test-case "Overwrite project settings"
      (write-project-settings "test-project" #hasheq((A . 22)))
      (check-equal? (read-project-settings "test-project") #hasheq((A . 22))))

    (test-case "Update project settings"
      (write-project-settings/key "test-project" 'A 55)
      (write-project-settings/key "test-project" 'boost "boost")
      (check-equal? (read-project-settings "test-project") #hasheq((A . 55) (boost . "boost"))))

    (test-case "Fetch template (from HTTP)"
      (new-project-from "test-project-template-http" "https://github.com/cs136/seashell-default/archive/v1.0.zip")
      (check-true (file-exists? (build-path (build-project-path "test-project-template-http") "default/main.c"))))

    (test-case "Fetch template (from URL, file)"
      (new-project-from "test-project-template-file-url" (format "file://~a/src/tests/template.zip" SEASHELL_SOURCE_PATH))
      (check-true (file-exists? (build-path (build-project-path "test-project-template-file-url") "default/main.c"))))

    (test-case "Fetch template (from file)"
      (new-project-from "test-project-template-file" (format "~a/src/tests/template.zip" SEASHELL_SOURCE_PATH))
      (check-true (file-exists? (build-path (build-project-path "test-project-template-file") "default/main.c"))))

    (test-case "Test Racket files (from template)"
      (new-project-from "test-racket" (format "~a/src/tests/test-template.zip" SEASHELL_SOURCE_PATH))
      (define-values (success hsh) (compile-and-run-project "test-racket" "Test/Test.rkt" '("Test")))
      (check-true success)
      (for ([pid (hash-ref hsh 'pids)]
            [exp-result '("passed")])
        (sync (program-wait-evt pid))
        (check-equal? exp-result (third (deserialize (read (program-stdout pid)))))))

    (test-case "Test Marmoset Racket Files (from template)"
      (new-project-from "test-marmoset-racket" (format "~a/src/tests/test-marmoset-racket.zip" SEASHELL_SOURCE_PATH))
      (define-values (success hsh) (compile-and-run-project (build-project-path "test-marmoset-racket") "Test.rkt" '("Test") #t 'flat))
      (check-true success)
      (for ([pid (hash-ref hsh 'pids)]
            [exp-result '("passed")])
        (sync (program-wait-evt pid))
        (check-equal? exp-result (third (deserialize (read (program-stdout pid)))))))

    (test-case "Test Marmoset Racket Files (from runner/CLI)"
      (parameterize ([current-directory (format "~a/src/tests/marmoset-racket-test-runner" SEASHELL_SOURCE_PATH)])
        (define-values (success hsh) (compile-and-run-project "." "Test.rkt" '("Test") #t 'current-directory))
        (check-true success)
        (for ([pid (hash-ref hsh 'pids)]
              [exp-result '("passed")])
          (sync (program-wait-evt pid))
          (check-equal? exp-result (third (deserialize (read (program-stdout pid))))))))

    (test-case "Test template does not overwrite existing project."
      (check-exn exn:fail? (thunk (new-project-from "test-project-template-file" (format "~a/src/tests/template.zip" SEASHELL_SOURCE_PATH)))))
    ))
