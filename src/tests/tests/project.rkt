#lang racket

(require rackunit
         seashell/backend/project
         seashell/backend/runner)

(define test-dir (string->path "./.seashell-test"))

(define/provide-test-suite project-test
  (test-suite "Project Tests"
    (test-case "Create a Project"
      (new-project "foo")
      (check is-project? "foo"))

    (test-case "Delete a non-Project"
      (check-exn exn:fail? (thunk (delete-project "bar"))))

    (test-case "Run a Project"
      (with-output-to-file (check-and-build-path (build-project-path "bar") "test.c")
        (thunk (display "#include <stdlib.h>\nint main() {\nprintf(\"Hello.\");\n}\n")))
      (define run-pid (run-project "bar" "test.c" #f))
      (sync (program-wait-evt run-pid)))

    (test-case "Export a Project"
      (export-project "bar"))

    (test-case 

;; Test 6
;; try listing the projects when there are none
(delete-project "bar")

(if (equal? (list-projects) '())
  (set! passed-tests (add1 passed-tests))
  (display "list-projects is not working.\n" (current-error-port)))

(printf "~a\n~a\n" total-tests passed-tests)
