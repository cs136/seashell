#lang racket

(require rackunit
         rackunit/text-ui
         errortrace
         "tests/test-environment.rkt"
         "tests/files.rkt"
         "tests/project.rkt"
         "tests/crypto.rkt")

(setup-test-environment)
;; Run tests
(define result
  (run-tests 
    (make-test-suite "all-tests"
      (list
        file-suite
        project-suite
        crypto-suite))))
(teardown-test-environment)

(exit result)
