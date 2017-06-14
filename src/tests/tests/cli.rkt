#lang racket

(require seashell/seashell-config
         seashell/backend/project
         rackunit)

(define seashell-cli-exe (build-path SEASHELL_BUILD_PATH "src/collects/seashell-cli/seashell-cli"))

(define/contract (seashell-cli . params)
  (->* () #:rest (listof string?) integer?)
  (apply system*/exit-code (cons seashell-cli-exe params)))

(define/contract (cli-marmtest code in expect [timeout #f])
  (->* (string? string? string?) ((or/c #f integer?)) integer?)
  (dynamic-wind
    (thunk (with-output-to-file "main.c" (thunk (display code)))
           (with-output-to-file "main.in" (thunk (display in)))
           (with-output-to-file "main.expect" (thunk (display expect))))
    (thunk (if timeout
               (seashell-cli "marmtest" "-t" (number->string timeout) "." "main.c" "main" "out" "err")
               (seashell-cli "marmtest" "." "main.c" "main" "out" "err")))
    (thunk (map delete-file '("main.c" "main.in" "main.expect")))))

(define/contract (cli-object code [name "object"])
  (->* (string?) (string?) integer?)
  (define src (string-append name ".c"))
  (dynamic-wind
    (thunk
      (with-output-to-file src
        (thunk (display code))))
    (thunk
      (seashell-cli "object" src))
    (thunk
      (delete-file src))))

(define old-dir #f)

(define/provide-test-suite seashell-cli-suite
  #:before (thunk (make-directory (build-path (read-config 'seashell) "cli-files"))
                  (set! old-dir (current-directory))
                  (current-directory (build-path (read-config 'seashell) "cli-files")))
  #:after (thunk (delete-directory/files (build-path (read-config 'seashell) "cli-files"))
                 (current-directory old-dir))
  (test-suite "seashell-cli test suite"
    (test-case "A program fails to compile"
      (printf "seashell-cli: ~a\nexists: ~a\n" seashell-cli-exe (file-exists? seashell-cli-exe))
      (check-equal?
        (cli-marmtest "good code;\n" "" "")
        10))

    (test-case "A program crashes at runtime"
      (check-equal?
        (cli-marmtest "#include <stdio.h>\nint main(){int *p=NULL;printf(\"%d\\n\",*p);}"
                      "" "")
        20))

    (test-case "A program fails an assertion"
      (check-equal?
        (cli-marmtest "#include <assert.h>\nint main(){assert(0);}"
                      "" "")
        21))

    (test-case "A program fails the test"
      (check-equal?
        (cli-marmtest "#include <stdio.h>\nint main(){int x;scanf(\"%d\",&x);printf(\"%d\\n\",x);}"
                      "42\n" "43\n")
        30))

    (test-case "A program passes the test"
      (check-equal?
        (cli-marmtest "#include <stdio.h>\nint main(){int x;scanf(\"%d\",&x);printf(\"%d\\n\",x);}"
                      "42\n"
                      "42\n")
        40))

    (test-case "A program times out"
      (check-equal?
        (cli-marmtest "int main(){while(1){}}" "" "" 1)
        50))

    (test-case "Generate a .ll file and run a project with it"
      (new-project "beep")
      (define q1-path (build-path (build-project-path "beep") "q1"))
      (make-directory q1-path)
      (check-equal?
        (cli-object "int magic() { return 42; }\n" (path->string (build-path q1-path "magic")))
        0)
      (with-output-to-file (build-path q1-path "magic.h")
        (thunk (display "int magic();\n")))
      (with-output-to-file (build-path q1-path "main.c")
        (thunk (display "#include <stdio.h>\n#include \"magic.h\"\nint main() { printf(\"%d\\n\", magic()); }\n")))
      (define-values (suc res)
        (compile-and-run-project "beep" "q1/main.c" "q1" '()))
      (check-true suc)
      (delete-project "beep"))
  ))
