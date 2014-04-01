#lang racket

(require seashell/seashell-config
         seashell/backend/project
         seashell/backend/files)

(define total-tests 6)
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

;; Test 2: read an empty file

(if (equal? (read-file "test" "good.c") #"")
  (set! passed (add1 passed))
  (display "Failed to read from file.\n" (current-error-port)))

;; Test 3: write to file/read non-empty file

(write-file "test" "good.c" #"foobar")

(if (equal? (read-file "test" "good.c") #"foobar")
  (set! passed (add1 passed))
  (display "Failed to write to file.\n" (current-error-port)))

;; Test 4: rename a file

(with-handlers ([exn:project? (lambda (e)
  (display (exn-message e) (current-error-port)))])
  (rename-file "test" "good.c" "bad.c"))

(if (file-exists? (check-and-build-path (build-project-path "test") "bad.c"))
  (set! passed (add1 passed))
  (display "Failed to rename a file.\n" (current-error-port)))

;; Test 5: list the files

(new-file "test" "good.c")

(match (list-files "test")
  [(list "good.c" "bad.c") (set! passed (add1 passed))]
  [_ (display "Failed to list files.\n" (current-error-port))])

;; Test 6: remove a file

(with-handlers ([exn? (lambda (e)
  (display (exn-message e) (current-error-port)))])
  (remove-file "test" "bad.c"))

(if (file-exists? (check-and-build-path (build-project-path "test") "bad.c"))
  (display "Failed to remove file.\n" (current-error-port))
  (set! passed (add1 passed)))

;; get rid of the project
(delete-project "test")

(printf "~a\n~a\n" total-tests passed)
