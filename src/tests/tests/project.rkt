#lang racket

(require "test-environment.rkt"
         seashell/backend/project
         seashell/backend/runner)

(define total-tests 7)
(define passed-tests 0)

(define test-dir (string->path "./.seashell-test"))

;; Test 1
;; make a project
(new-project "foo")
(if (is-project? "foo")
  (set! passed-tests (add1 passed-tests))
  (display "Project not successfully created.\n" (current-error-port)))

;; Test 2
;; delete a non-project
(with-handlers ([exn:fail?
  (lambda (e) (set! passed-tests (add1 passed-tests)))])
  (delete-project "bar")
  (display "Was able to delete non-existent project.\n"
    (current-error-port)))

;; Test 3
;; list projects when there is more than one
(new-project "bar")

(if (equal? (sort (list-projects) string<?) '("bar" "foo"))
  (set! passed-tests (add1 passed-tests))
  (display "list-projects failed.\n" (current-error-port)))

;; Test 4
;; delete an actual project
(delete-project "foo")
(if (is-project? "foo")
  (display "Was unable to delete a project.\n" (current-error-port))
  (set! passed-tests (add1 passed-tests)))

;; Test 5
;; Run a project
(with-output-to-file (check-and-build-path (build-project-path "bar") "test.c")
  (thunk (display "#include <stdlib.h>\nint main() {\nprintf(\"Hello.\");\n}\n")))

(with-handlers
  ([exn? (lambda (e) (display (exn-message e) (current-error-port)))])
  (define run-pid (run-project "bar" "test.c" #f))
  (sync (program-wait-evt run-pid))
  (set! passed-tests (add1 passed-tests)))

;; Test 6
;; Export project
(with-handlers
  ([exn:fail? (lambda (e) (display (exn-message e) (current-error-port)))])
  (export-project "bar")
  (set! passed-tests (add1 passed-tests)))

;; Test 6
;; try listing the projects when there are none
(delete-project "bar")

(if (equal? (list-projects) '())
  (set! passed-tests (add1 passed-tests))
  (display "list-projects is not working.\n" (current-error-port)))

(printf "~a\n~a\n" total-tests passed-tests)
