#lang racket

(require seashell/db/tools
         seashell/db/database
         seashell/backend/files
         seashell/backend/project
         seashell/seashell-config
         rackunit)

(define/provide-test-suite db-files-suite
  (test-suite "Files suite with SQLite backend"
    #:before init-sync-database
    #:after clear-sync-database
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

    (test-case "Fetch template (HTTP)"
      (define pid (new-project "template-http" "https://github.com/cs136/seashell-default/archive/v1.0.zip"))
      (export-project pid #f "export")
      (check-pred file-exists? "export/default/main.c")
      (delete-directory/files "export"))

    (test-case "Fetch template (file URL)"
      (define pid (new-project "template-file-url"
        (format "file://~a/src/tests/template.zip" SEASHELL_SOURCE_PATH)))
      (export-project pid #f "export")
      (check-pred file-exists? "export/default/main.c")
      (delete-directory/files "export"))

    (test-case "Run a project"
      (define pid (new-project "run-project" #f `#hasheq((q1_runner_file . "q1/run.c"))))
      (define did (new-directory pid "q1"))
      (define-values (fid cid) (new-file pid "q1/run.c" "int main(void) {\n  return 0;\n}\n" 0))
      (define-values (res hsh) (compile-and-run-project/db pid "q1" '()))
      (check-true res))

    (test-case "Run a project with common run file"
      (define pid (new-project "run-project-common" #f `#hasheq((q1_runner_file . "common/prog.c"))))
      (define did (new-directory pid "common"))
      (define-values (fid cid) (new-file pid "common/prog.c" "int main(void) {\n  return 0;\n}\n" 0))
      (define-values (res hsh) (compile-and-run-project/db pid "q1" '()))
      (check-true res))

    (test-case "Run a project with tests"
      (define pid (new-project "run-project-tests" #f `#hasheq((q1_runner_file . "q1/main.c"))))
      (define did (new-directory pid "q1"))
      (new-file pid "q1/main.c" "#include <stdio.h>\nint main(void) {\n  printf(\"Hello world!\\n\");\n}\n" 0)
      (new-directory pid "q1/tests")
      (new-file pid "q1/tests/simple.in" "" 0)
      (new-file pid "q1/tests/simple.expect" "Hello world!\n" 0)
      (define-values (res hsh) (compile-and-run-project/db pid "q1" '("simple")))
      (check-true res))
    ))
