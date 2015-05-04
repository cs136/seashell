#lang racket

(require rackunit
         seashell/backend/project
         seashell/backend/runner
         seashell/seashell-config)

(define/provide-test-suite project-suite
  (test-suite "Project Tests"
    (test-case "Create a Project"
      (new-project "foo")
      (check-pred is-project? "foo"))
    
    (test-case "Lock a project"
      (sleep 2) ;; make sure we have enough delay to make sure timestamps
                ;; will be updated correctly.
      (define current-timestamp (file-or-directory-modify-seconds
                                  (build-project-path "foo")))
      (check-true (lock-project "foo" (current-thread)))
      (define new-timestamp (file-or-directory-modify-seconds
                             (build-project-path "foo")))
      (check < current-timestamp new-timestamp))
    
    (test-case "Lock a locked project"
      (sleep 2) ;; make sure we have enough delay to make sure timestamps
                ;; will be updated correctly.
      (define current-timestamp (file-or-directory-modify-seconds
                                  (build-project-path "foo")))
      (sync (thread (thunk
        (check-false (lock-project "foo" (current-thread))))))
      (define new-timestamp (file-or-directory-modify-seconds
                             (build-project-path "foo")))
      (check-equal? current-timestamp new-timestamp))

    (test-case "Force lock a locked project"
      (sleep 2) ;; make sure we have enough delay to make sure timestamps
                ;; will be updated correctly.
      (define current-timestamp (file-or-directory-modify-seconds
                                  (build-project-path "foo")))
      (sync (thread (thunk
        (force-lock-project "foo" (current-thread)))))
      (define new-timestamp (file-or-directory-modify-seconds
                             (build-project-path "foo")))
      (check < current-timestamp new-timestamp))

    (test-case "Delete a non-Project"
      (check-exn exn:fail? (thunk (delete-project "bar"))))

    (test-case "Run a Project"
      (with-output-to-file (check-and-build-path (build-project-path "foo") "test.c")
        (thunk (display "#include <stdio.h>\nint main() {\nprintf(\"Hello.\");\n}\n")))
      (define-values (success hsh) (compile-and-run-project "foo" "test.c" '()))
      (check-true success)
      (sync (program-wait-evt (hash-ref hsh 'pid))))

    (test-case "Get a Compilation Error"
      (with-output-to-file (check-and-build-path (build-project-path "foo") "error.c")
        (thunk (display "great code;")))
      (define-values (res hsh) (compile-and-run-project "foo" "error.c" '()))
      (check-false res)
      (check string=? (hash-ref hsh 'status) "compile-failed"))

    (test-case "Export a Project"
      (export-project "foo"))

    (test-case "Most Recently Used"
      (update-most-recently-used "foo" #f '("fexists" "error.c") "some data")
      (check string=? (get-most-recently-used "foo" #f) "some data"))

    (test-case "Delete a Project"
      (delete-project "foo")
      (check-equal? (list-projects) '()))

    (test-case "Archive Projects"
      (new-project "bar")
      (new-project "foobar")
      (archive-projects "my-archive")
      (check-true (directory-exists? (build-path (read-config 'seashell) "archives" "my-archive")))
      (check-true (directory-exists? (build-path (read-config 'seashell) "archives" "my-archive" "bar")))
      (check-true (directory-exists? (build-path (read-config 'seashell) "archives" "my-archive" "foobar"))))))
