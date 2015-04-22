#lang racket

(require rackunit
         rackunit/text-ui
         errortrace
         "tests/test-environment.rkt"
         "tests/files.rkt")

(run-tests (make-test-suite "all-tests"
  (list
    file-suite)))
