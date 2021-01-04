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
      (make-directory (check-and-build-path (build-project-path "foo") "q1"))
      (with-output-to-file (check-and-build-path (build-project-path "foo") "q1" "test.c")
        (thunk (display "#include <stdio.h>\nint main() {\nprintf(\"Hello.\");\n}\n")))
      (define-values (success hsh) (compile-and-run-project "foo" "q1/test.c" "q1" '()))
      (check-true success)
      (sync (program-wait-evt (hash-ref hsh 'pid))))

    (test-case "Run a Project with common and tests"
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
      (define-values (success hsh) (compile-and-run-project "foo" "q1/main.c" "q1" '("pass" "fail" "crash") #f))
      (check-true success)
      (for ([pid (hash-ref hsh 'pids)]
            [exp-result '("passed")])
        (sync (program-wait-evt pid))
        (check-equal? exp-result (third (deserialize (read (program-stdout pid)))))))

    (test-case "Run a project with main file in common dir"
      (with-output-to-file (check-and-build-path (build-project-path "foo") "common" "common-main.c")
        (thunk (display "int main() { }\n")))
      (define-values (res hsh) (compile-and-run-project "foo" "common/common-main.c" "q1" '()))
      (check-true res)
      (sync (program-wait-evt (hash-ref hsh 'pid))))

    (test-case "Get a Compilation Error"
      (with-output-to-file (check-and-build-path (build-project-path "foo") "q1" "error.c")
        (thunk (display "great code;")))
      (define-values (res hsh) (compile-and-run-project "foo" "error.c" "q1" '()))
      (check-false res)
      (check string=? (hash-ref hsh 'status) "compile-failed"))

    (test-case "Export a Project"
      (export-project "foo"))

    (test-case "Most Recently Used"
      (define dpath (check-and-build-path (build-project-path "foo") "testdir"))
      (unless (directory-exists? dpath) (make-directory dpath))
      (update-most-recently-used "foo" #f "testdir")
      (check string=? (get-most-recently-used "foo" #f) "testdir")
      (update-most-recently-used "foo" #f "testdir2")
      (check-false (get-most-recently-used "foo" #f)))

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

    (test-case "update nonexistent project settings"
      (new-project "test-project-2")
      (write-project-settings/key "test-project-2" 'key "val")
      (define settings (read-project-settings "test-project-2"))
      (check-equal? (hash-ref settings 'key) "val"))

    (test-case "Write project settings"
      (new-project "test-project")
      (write-project-settings "test-project" #hasheq((A . 5) (B . 6)))
      (define settings (read-project-settings "test-project"))
      (check-equal? (hash-ref settings 'A) 5)
      (check-equal? (hash-ref settings 'B) 6))

    (test-case "Overwrite project settings"
      (write-project-settings "test-project" #hasheq((A . 22)))
      (define settings (read-project-settings "test-project"))
      (check-equal? (hash-ref settings 'A) 22))

    (test-case "Update project settings"
      (write-project-settings/key "test-project" 'A 55)
      (write-project-settings/key "test-project" 'boost "boost")
      (define settings (read-project-settings "test-project"))
      (check-equal? (hash-ref settings 'A) 55)
      (check-equal? (hash-ref settings 'boost) "boost"))

    (test-case "Fetch template (from HTTP)"
      (new-project-from "test-project-template-http" "https://student.cs.uwaterloo.ca/~cs136/seashell-project-template.zip")
      (check-true (file-exists? (build-path (build-project-path "test-project-template-http") "default/main.c"))))

    (test-case "Fetch template (from URL, file)"
      (new-project-from "test-project-template-file-url" (format "file://~a/src/tests/template.zip" SEASHELL_SOURCE_PATH))
      (check-true (file-exists? (build-path (build-project-path "test-project-template-file-url") "default/main.c"))))

    ;; TODO: Include a test for fetching template via SSH
    (test-case "Fetch template from SSH and fail"
      (check-true
        (with-handlers
          ([exn:fail? (thunk* #t)])
            (new-project-from "test-project-template-ssh-fail"
              "ssh://nobody@nonexistant.host:/home/seashell/template.zip")
            #f)))

    (test-case "Fetch template (from file)"
      (new-project-from "test-project-template-file" (format "~a/src/tests/template.zip" SEASHELL_SOURCE_PATH))
      (check-true (file-exists? (build-path (build-project-path "test-project-template-file") "default/main.c"))))

    (test-case "Test Racket files (from template)"
      (new-project-from "test-racket" (format "~a/src/tests/test-template.zip" SEASHELL_SOURCE_PATH))
      (define-values (success hsh) (compile-and-run-project "test-racket" "Test/Test.rkt" "Test" '("Test")))
      (check-true success)
      (for ([pid (hash-ref hsh 'pids)]
            [exp-result '("passed")])
        (sync (program-wait-evt pid))
        (check-equal? exp-result (third (deserialize (read (program-stdout pid)))))))

    (test-case "Test Marmoset Racket Files (from template)"
      (new-project-from "test-marmoset-racket" (format "~a/src/tests/test-marmoset-racket.zip" SEASHELL_SOURCE_PATH))
      (define-values (success hsh) (compile-and-run-project (build-project-path "test-marmoset-racket") "Test/Test.rkt" "Test" '("Test") #t 'flat))
      (check-true success)
      (for ([pid (hash-ref hsh 'pids)]
            [exp-result '("passed")])
        (sync (program-wait-evt pid))
        (check-equal? exp-result (third (deserialize (read (program-stdout pid)))))))

    (test-case "Test template does not overwrite existing project."
      (check-exn exn:fail? (thunk (new-project-from "test-project-template-file" (format "~a/src/tests/template.zip" SEASHELL_SOURCE_PATH)))))
    ))
