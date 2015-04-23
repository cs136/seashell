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
      (define-values (_ hsh) (compile-and-run-project "foo" "test.c" '()))
      (sync (program-wait-evt (hash-ref hsh 'pid))))

    (test-case "Export a Project"
      (export-project "foo"))

    (test-case "Delete a Project"
      (delete-project "foo")
      (check-equal? (list-projects) '()))))
