#lang racket

(require seashell/seashell-config
         rackunit)

(define seashell-cli-exe (build-path SEASHELL_BUILD_PATH "src/collects/seashell-cli"))

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

(define/provide-test-suite seashell-cli-suite
  #:before (thunk (make-directory (build-path (read-config 'seashell) "cli-files"))
                  (current-directory (build-path (read-config 'seashell) "cli-files")))
  #:after (thunk (delete-directory/files (build-path (read-config 'seashell) "cli-files")))
  (test-suite "seashell-cli test suite"
    (test-case "A program fails to compile"
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
  ))
