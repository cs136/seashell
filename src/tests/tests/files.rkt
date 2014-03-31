#lang racket

(require seashell/seashell-config
         seashell/backend/project
         seashell/backend/files)

(define total-tests 3)
(define passed 0)

(define test-dir (string->path "./.seashell-test"))

(when (directory-exists? test-dir)
  (delete-directory/files test-dir))

(make-directory test-dir)
(config-set! 'test-mode #t)
(config-set! 'seashell test-dir)
(new-project "test")

;; Test 1: create a file

(new-file "test" "good.c")

(if (file-exists? (check-and-build-path (build-project-path "test") "good.c"))
  (set! passed (add1 passed))
  (display "Failed to create a new file.\n" (current-error-port)))


;; Test 2: rename a file

(with-handlers ([exn:project? (lambda (e)
  (display (exn-message e) (current-error-port)))])
  (rename-file "test" "good.c" "bad.c"))

(if (file-exists? (check-and-build-path (build-project-path "test") "bad.c"))
  (set! passed (add1 passed))
  (display "Failed to rename a file.\n" (current-error-port)))

;; Test 3: remove a file

(with-handlers ([exn? (lambda (e)
  (display (exn-message e) (current-error-port)))])
  (remove-file "test" "bad.c"))

(if (file-exists? (check-and-build-path (build-project-path "test") "bad.c"))
  (display "Failed to remove file.\n" (current-error-port))
  (set! passed (add1 passed)))

;; get rid of the project
(delete-project "test")

(printf "~a\n~a\n" total-tests passed)
