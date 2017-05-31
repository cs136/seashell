#lang racket

(require seashell/db/seashell
         seashell/backend/db-files
         rackunit)

(define/provide-test-suite db-files-suite
  (test-suite "Files suite with SQLite backend"
    #:before (thunk (init-database))
    (test-case "Create simple project"
      (define pid (new-project "A1"))
      (define did (new-directory pid "q1"))
      (define-values (fid cid) (new-file pid "q1/main.c" "int main(void) {\n  return 0;\n}\n" 0))
      (export-project pid #f "export")
      (check-pred file-exists? "export/q1/main.c")
      (delete-directory/files "export")
      (export-project pid #t "proj.zip")
      (check-pred file-exists? "proj.zip")
      (delete-file "proj.zip"))

    (test-case "Create, delete, export"
      (define pid (new-project "Tut00"))
      (define did (new-directory pid "example"))
      (define-values (fid cid) (new-file pid "example/main.c" "int main(void) { }\n" 0))
      (delete-project pid)
      (export-project pid #f "export")
      (check-true (not (file-exists? "export/example/main.c")))
      (delete-directory/files "export"))
    ))
