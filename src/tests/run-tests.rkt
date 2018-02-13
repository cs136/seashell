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
         "tests/offline.rkt")

(setup-test-environment)
;; Run tests
(define result
  (run-tests
    (make-test-suite "all-tests"
      (list
        config-suite
        compiler-suite
        file-suite
        project-suite
        crypto-suite
        asan-parser-suite
        seashell-cli-suite
        offline-suite
        ))))
(teardown-test-environment)

(exit result)
