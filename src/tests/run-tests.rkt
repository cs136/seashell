#lang racket

(require rackunit
         rackunit/text-ui
         errortrace
         "tests/test-environment.rkt"
         "tests/files.rkt"
         "tests/project.rkt"
         "tests/crypto.rkt"
         "tests/compiler.rkt"
         "tests/asan-error-parse.rkt"
         "tests/config.rkt"
         "tests/cli.rkt"
         "tests/offline.rkt"
         "tests/database.rkt"
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
        file-suite
        project-suite
        crypto-suite
        asan-parser-suite
        seashell-cli-suite
        offline-suite
        database-suite
        db-files-suite
        ))))
(teardown-test-environment)

(exit result)
