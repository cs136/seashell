#lang racket

(require rackunit
         "tests/test-environment.rkt"
         "tests/files.rkt")

(run-tests (make-test-suite
  file-tests))
