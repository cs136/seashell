#lang racket

(require rackunit
         rackunit/text-ui
         errortrace
         "tests/test-environment.rkt"
         "tests/crypto.rkt"
         "tests/compiler.rkt"
         "tests/asan-error-parse.rkt"
         "tests/config.rkt"
         "tests/cli.rkt"
         "tests/db.rkt"
         "tests/db-files.rkt")

(setup-test-environment)
;; Run tests
(define result
  (run-tests
    (make-test-suite "all-tests"
      (list
        config-suite
        asan-parser-suite
        compiler-suite
        crypto-suite
        asan-parser-suite
        seashell-cli-suite
        database-suite
        db-files-suite
        ))))
(teardown-test-environment)

(exit result)
