#lang racket

(require rackunit
         racket/serialize
         seashell/backend/project
         seashell/backend/runner
         seashell/seashell-config)

(define test-hdr-file "int add(int, int);\n")
(define test-imp-file "#include \"add.h\"\nint add(int a, int b){ return a+b; }\n")
(define test-main-file #<<HERE
#include <stdio.h>
#include "add.h"
int main(){
    printf("%d\n", add(3,4));
}
HERE
)

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

    (test-case "Run a Project with tests"
      (for ([file '("add.h" "add.c" "main.c" "tests/a.in" "tests/a.expect")]
            [contents (list test-hdr-file test-imp-file test-main-file "3\n4\n" "7\n")])
        (with-output-to-file (check-and-build-path (build-project-path "foo") file)
          (thunk (display contents))))
      (define-values (success hsh) (compile-and-run-project "foo" "main.c" (list "a")))
      (check-true success)
      (sync (program-wait-evt (hash-ref hsh 'pid)))
      (check-true (match (sync (wrap-evt (program-stdout (hash-ref hsh 'pid)) (compose deserialize read)))
          [(list _ _ "passed" _ _) #t]
          [_ #f])))

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
      (check-true (directory-exists? (build-path (read-config 'seashell) "archives" "my-archive" "foobar"))))
    
    
    (test-case "Read from nonexistent project settings"
      (new-project "test-project")
      (check-false (read-project-settings "test-project")))

    (test-case "update nonexistent project settings"
      (new-project "test-project-2")
      (write-project-settings/key "test-project-2" 'key "val")
      (check-equal? (read-project-settings "test-project-2") #hasheq((key . "val"))))

    (test-case "Write project settings"
      (write-project-settings "test-project" #hasheq((A . 5) (B . 6)))
      (check-equal? (read-project-settings "test-project") #hasheq((A . 5) (B . 6))))

    (test-case "Overwrite project settings"
      (write-project-settings "test-project" #hasheq((A . 22)))
      (check-equal? (read-project-settings "test-project") #hasheq((A . 22))))

    (test-case "Update project settings"
      (write-project-settings/key "test-project" 'A 55)
      (write-project-settings/key "test-project" 'boost "boost")
      (check-equal? (read-project-settings "test-project") #hasheq((A . 55) (boost . "boost"))))   

    ))
