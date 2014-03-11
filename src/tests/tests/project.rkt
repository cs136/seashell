#lang racket

(require seashell/seashell-config
         seashell/backend/project)

(define total-tests 4)
(define passed-tests 0)

(define test-dir (string->path "./.seashell-test"))

;; Test 1
;; test check-path with bad path
#|(with-handlers ([exn
    (lambda (e) (set! passed-tests (add1 passed-tests)))])
  (check-path (string->path "./!@#$/directory"))
  (display "check-path accepted garbage.\n" (current-error-port)))|#

(if (directory-exists? test-dir)
  (delete-directory/files test-dir)
  (void))
(make-directory test-dir)
(config-set! 'seashell test-dir)
;; Test 2
;; make a project
(new-project "foo")
(if (is-project? "foo")
  (set! passed-tests (add1 passed-tests))
  (display "Project not successfully created.\n" (current-error-port)))

;; Test 3
;; delete a non-project
(with-handlers ([exn:project?
  (lambda (e) (set! passed-tests (add1 passed-tests)))])
  (delete-project "bar")
  (display "Was able to delete non-existent project.\n"
    (current-error-port)))

;; Test 4
;; delete an actual project
(delete-project "foo")
(if (is-project? "foo")
  (display "Was unable to delete a project.\n")
  (set! passed-tests (add1 passed-tests)))

;; Test 5
;; try listing the projets (should be no projects at this point)
(if (equal? (list-projects) '())
  (set! passed-tests (add1 passed-tests))
  (display "list-projects is not working.\n" (current-error-port)))

(printf "~a\n~a\n" total-tests passed-tests)
