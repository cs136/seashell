#lang racket

(require seashell/backend/project
         seashell/backend/files
         rackunit)

(define/provide-test-suite file-suite
  (test-suite "File test suite"
    #:before (thunk (new-project "test"))
    #:after (thunk (delete-project "test"))
    (test-case "Create a file"
      (new-file "test" "good.c" #"" 'raw #f)
      (check-pred file-exists? (check-and-build-path (build-project-path "test") "good.c")))

    (test-case "Read a file"
      (check-equal? (read-file "test" "good.c") #"")
      (write-file "test" "good.c" #"foobar")
      (check-equal? (read-file "test" "good.c") #"foobar"))

    (test-case "Rename a file"
      (rename-file "test" "good.c" "bad.c")
      (check-pred file-exists? (check-and-build-path (build-project-path "test") "bad.c"))
      (check-false (file-exists? (check-and-build-path (build-project-path "test") "good.c"))))
    
    (test-case "List files"
      (new-file "test" "good.c" #"" 'raw #f)
      (check-match (list-files "test") (list-no-order
        (list "default" #t _)
        (list "default/main.c" #f _)
        (list "good.c" #f _)
        (list "bad.c" #f _))))

    (test-case "Delete a file"
      (remove-file "test" "bad.c")
      (check-false (file-exists? (check-and-build-path (build-project-path "test") "bad.c"))))))
