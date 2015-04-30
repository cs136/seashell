#lang racket

(require seashell/seashell-config
         seashell/compiler/place
         seashell/backend/project)

(provide setup-test-environment
         teardown-test-environment)

(define (setup-test-environment)
  ;; Common file for setting up the testing environment
  (define test-dir (build-path (find-system-path 'temp-dir)
                               (format "seashell-test-~a" (random 10000))))

  ;; Set up test environment.
  (if (directory-exists? test-dir)
    (delete-directory/files test-dir)
    (void))
  (make-directory test-dir)
  (config-set! 'seashell test-dir)
  (config-set! 'test-mode #t)
  (init-projects)
  
  ;; Start places. (note: with default config here)
  (seashell-compile-place/init))

(define (teardown-test-environment)
  (delete-directory/files (read-config 'seashell)))

