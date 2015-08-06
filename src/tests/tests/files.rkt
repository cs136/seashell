#lang racket

(require seashell/backend/project
         seashell/backend/files
         rackunit
         openssl/md5)

(define/provide-test-suite file-suite
  (test-suite "File test suite"
    #:before (thunk (new-project "test"))
    #:after (thunk (delete-project "test"))
    (test-case "Create a file"
      (new-file "test" "good.c" #"" 'raw #f)
      (check-pred file-exists? (check-and-build-path (build-project-path "test") "good.c")))

    (test-case "Read a file"
      (define-values (data checksum) (read-file "test" "good.c"))
      (check-equal? data #"")
      (write-file "test" "good.c" #"foobar")
      (define-values (data2 checksum2) (read-file "test" "good.c"))
      (check-equal? data2 #"foobar"))

    (test-case "Rename a file"
      (rename-file "test" "good.c" "bad.c")
      (check-pred file-exists? (check-and-build-path (build-project-path "test") "bad.c"))
      (check-false (file-exists? (check-and-build-path (build-project-path "test") "good.c"))))
    
    (test-case "List files"
      (new-file "test" "good.c" #"" 'raw #f)
      (check-match (list-files "test") (list-no-order
        (list "default" #t _ #f)
        (list "default/main.c" #f _ _)
        (list "good.c" #f _ _)
        (list "bad.c" #f _ _))))
    
    (test-case "Create a file, with a data URL"
      (new-file "test" "foo1.c" #"data:,A brief note" 'url #f)
      (define-values (data checksum) (read-file "test" "foo1.c"))
      (check-equal? data #"A brief note"))

    (test-case "Create a file, with a data URL (base64)"
      (new-file "test" "foo2.c" #"data:text/html;base64,VGhpcyBpcyBhIHRlc3QK" 'url #f)
      (define-values (data checksum) (read-file "test" "foo2.c"))
      (check-equal? data #"This is a test\n"))
    
    (test-case "Create a file, with a data URL (base64) (missing MIME)"
      (new-file "test" "foo3.c" #"data:;base64,VGhpcyBpcyBhIHRlc3QK" 'url #f)
      (define-values (data checksum) (read-file "test" "foo3.c"))
      (check-equal? data #"This is a test\n"))
    
    (test-case "Create a file, with a data URL (base64) (permissive)"
      (new-file "test" "foo4.c" #"data:base64,VGhpcyBpcyBhIHRlc3QK" 'url #f)
      (define-values (data checksum) (read-file "test" "foo4.c"))
      (check-equal? data #"This is a test\n"))

    (test-case "Create a file, check MD5 tag."
      (define contents #"Hello World!")
      (define tag (call-with-input-bytes contents md5)) 
      (check-equal? tag (new-file "test" "foo4.5.c" contents 'raw #f)))

    (test-case "Write to file, check MD5 tag."
      (define contents #"Hello World!")
      (define tag (call-with-input-bytes contents md5)) 
      (new-file "test" "foo5.c" contents 'raw #f)
      (write-file "test" "foo5.c" #"Hello World 2.0!" tag))
    
    (test-case "Write to file, check MD5 tag. (failure)"
      (define contents #"Hello World!")
      (define tag "not a md5 tag.") 
      (new-file "test" "foo6.c" contents 'raw #f)
      (check-exn exn:fail? (lambda () (write-file "test" "foo6.c" #"Hello World 2.0!" tag))))

    (test-case "Read file, check MD5 tag."
      (define contents #"Hello World!")
      (define tag (call-with-input-bytes contents md5)) 
      (new-file "test" "foo7.c" contents 'raw #f)
      (define-values (data checksum) (read-file "test" "foo7.c"))
      (check-equal? checksum tag))
    
    (test-case "Write file, check MD5 tag."
      (define contents #"Hello World!")
      (define tag (call-with-input-bytes contents md5)) 
      (new-file "test" "foo8.c" #"" 'raw #f)
      (define checksum (write-file "test" "foo8.c" contents #f))
      (check-equal? checksum tag))

    (test-case "Delete a file"
      (remove-file "test" "bad.c")
      (check-false (file-exists? (check-and-build-path (build-project-path "test") "bad.c"))))

    (test-case "Create a directory"
      (new-directory "test" "boost")
      (check-true (directory-exists? (check-and-build-path (build-project-path "test") "boost"))))

    (test-case "Delete a directory"
      (remove-directory "test" "boost")
      (check-false (directory-exists? (check-and-build-path (build-project-path "test") "boost"))))))
