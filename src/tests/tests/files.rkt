#lang racket

(require seashell/backend/project
         seashell/backend/files
         seashell/seashell-config
         openssl/md5
         rackunit)

(define/provide-test-suite file-suite
  (test-suite "File test suite"
    #:before (thunk (new-project "test"))
    #:after (thunk (delete-project "test"))
    (test-case "Create a file"
      (new-file "test" "good.c" #"" 'raw #f)
      (check-pred file-exists? (check-and-build-path (build-project-path "test") "good.c")))

    (test-case "Read a file"
      (define-values (pre-contents pre-history) (read-file "test" "good.c"))
      (check-equal? pre-contents #"")
      (write-file "test" "good.c" #"foobar" #"")
      (define-values (post-contents post-history) (read-file "test" "good.c"))
      (check-equal? post-contents #"foobar"))

    (test-case "Rename a file"
      (rename-file "test" "good.c" "bad.c")
      (check-pred file-exists? (check-and-build-path (build-project-path "test") "bad.c"))
      (check-false (file-exists? (check-and-build-path (build-project-path "test") "good.c"))))
    
    (test-case "List files"
      (new-file "test" "good.c" #"" 'raw #f)
      (new-file "test" ".hidden.c" #"" 'raw #f)
      (check-match (list-files "test") (list-no-order
        (list "default" #t _)
        (list "default/main.c" #f _)
        (list "good.c" #f _)
        (list "bad.c" #f _))))
    
    (test-case "Create a file, with a data URL"
      (new-file "test" "foo1.c" #"data:,A brief note" 'url #f)
      (define-values (contents history) (read-file "test" "foo1.c"))
      (check-equal? contents #"A brief note"))

    (test-case "Create a file, with a data URL (base64)"
      (new-file "test" "foo2.c" #"data:text/html;base64,VGhpcyBpcyBhIHRlc3QK" 'url #f)
      (define-values (contents history) (read-file "test" "foo2.c"))
      (check-equal? contents #"This is a test\n"))
    
    (test-case "Create a file, with a data URL (base64) (missing MIME)"
      (new-file "test" "foo3.c" #"data:;base64,VGhpcyBpcyBhIHRlc3QK" 'url #f)
      (define-values (contents history) (read-file "test" "foo3.c"))
      (check-equal? contents #"This is a test\n"))
    
    (test-case "Create a file, with a data URL (base64) (permissive)"
      (new-file "test" "foo4.c" #"data:base64,VGhpcyBpcyBhIHRlc3QK" 'url #f)
      (define-values (contents history) (read-file "test" "foo4.c"))
      (check-equal? contents #"This is a test\n"))

    ;; Normalizing newlines will ensure newline before EOF
    (test-case "Create a file, with a data URL and normalized newlines"
      (new-file "test" "foo5.c" #"data:,apple juice" 'url #t)
      (define-values (contents history) (read-file "test" "foo5.c"))
      (check-equal? contents #"apple juice\n"))

    (test-case "Create a file, with a data URL and already-normalized newlines"
      (new-file "test" "foo6.c" #"data:,apple juice\n\n" 'url #t)
      (define-values (contents history) (read-file "test" "foo6.c"))
      (check-equal? contents #"apple juice\n\n"))
    
    (test-case "Create a file, with a data URL and windows newlines"
      (new-file "test" "foo7.c" #"data:,apple juice\r\n" 'url #t)
      (define-values (contents history) (read-file "test" "foo7.c"))
      (check-equal? contents #"apple juice\n"))

    (test-case "Create and save a file, ensuring that the history exists"
      (new-file "test" "foo8.c" #"" 'raw #f)
      (write-file "test" "foo8.c" #"" #"sample history\n")
      (check-pred file-exists? (check-and-build-path (build-project-path "test") ".foo8.c.history"))
      (define-values (contents history) (read-file "test" "foo8.c"))
      (check-equal? history #"sample history\n"))

    (test-case "Delete a file"
      (remove-file "test" "bad.c")
      (check-false (or (file-exists? (check-and-build-path (build-project-path "test") "bad.c"))
                       (file-exists? (check-and-build-path (build-project-path "test") ".bad.c.history")))))

    (test-case "Create a directory"
      (new-directory "test" "boost")
      (check-true (directory-exists? (check-and-build-path (build-project-path "test") "boost"))))

    (test-case "Delete a directory"
      (remove-directory "test" "boost")
      (check-false (directory-exists? (check-and-build-path (build-project-path "test") "boost"))))

    (test-case "Revert file"
      (new-project-from "test-revert" (format "file://~a/src/tests/template.zip" SEASHELL_SOURCE_PATH))
      (check-true (file-exists? (build-path (build-project-path "test-revert") "default/main.c")))
      (remove-file "test-revert" "default/main.c")
      (check-false (file-exists? (build-path (build-project-path "test-revert") "default/main.c")))
      (check-equal? (restore-file-from-template "test-revert" "default/main.c" (format "file://~a/src/tests/template.zip" SEASHELL_SOURCE_PATH))
                    "262b1c12d6e84a11da92b605e16ecfd4")
      (remove-file "test-revert" "default/main.c")
      (new-file "test-revert" "default/main.c" #"foo" 'raw #f)
      (check-equal? (restore-file-from-template "test-revert" "default/main.c" (format "file://~a/src/tests/template.zip" SEASHELL_SOURCE_PATH))
                    "262b1c12d6e84a11da92b605e16ecfd4")
      (check-equal? (call-with-input-file (build-path (build-project-path "test-revert") "default/main.c") md5)
                    "262b1c12d6e84a11da92b605e16ecfd4"))
    ))
