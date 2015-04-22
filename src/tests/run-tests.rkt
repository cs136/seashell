#lang racket

(require rackunit
         rackunit/text-ui
         errortrace/errortrace-lib
         "tests/test-environment.rkt"
         "tests/files.rkt")

;; Run tests
(define result
  (run-tests 
    (make-test-suite "all-tests"
      (list
        file-suite))))

(exit result)
