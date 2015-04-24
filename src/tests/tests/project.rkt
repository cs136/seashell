#lang racket

(require rackunit
         seashell/backend/project
         seashell/backend/runner)

(define/provide-test-suite project-suite
  (test-suite "Project Tests"
    (test-case "Create a Project"
      (new-project "foo")
      (check-pred is-project? "foo"))

    (test-case "Delete a non-Project"
      (check-exn exn:fail? (thunk (delete-project "bar"))))

    (test-case "Run a Project"
      (with-output-to-file (check-and-build-path (build-project-path "foo") "test.c")
        (thunk (display "#include <stdio.h>\nint main() {\nprintf(\"Hello.\");\n}\n")))
      (define-values (success hsh) (compile-and-run-project "foo" "test.c" '()))
      (check-true success)
      (sync (program-wait-evt (hash-ref hsh 'pid))))

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
      (check-equal? (list-projects) '()))))
