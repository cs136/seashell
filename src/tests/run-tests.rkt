#lang racket

(require rackunit
         rackunit/text-ui
         errortrace
         "tests/test-environment.rkt"
         "tests/files.rkt"
         "tests/project.rkt")

;; Run tests
(define result
  (run-tests 
    (make-test-suite "all-tests"
      (list
        file-suite
        project-suite))))

(exit result)
