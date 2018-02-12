#lang racket

(require seashell/seashell-config
         seashell/backend/project
         racket/string
         rackunit)

;(define seashell-cli-exe (read-config 'seashell-cli))
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

(define/contract (cli-run main-file question-files [common-files '()] [stdin-contents ""] [verbose-flag #f])
  (->* (string? (listof (list/c string? string?)))
       ((listof (list/c string? string?)) string? boolean?)
       (values integer? string? string?))

  ;; Helper function to create files inside a folder
  (define/contract (create-files-in-folder files folder-name)
    (-> (listof (list/c string? string?)) string? void?)
    (make-directory folder-name)
    (for-each (lambda (pair)
                (define filename (first pair))
                (define contents (second pair))
                (with-output-to-file (build-path folder-name filename)
                  (thunk (display contents))))
              files))

  (dynamic-wind
   ;; Create folders and files
   (thunk (delete-directory/files "question" #:must-exist? #f)
          (delete-directory/files "common" #:must-exist? #f)
          (create-files-in-folder question-files "question")
          (create-files-in-folder common-files "common"))
   ;; Run the program
   (thunk (define-values (proc stdout stdin stderr)
            (apply subprocess #f #f #f seashell-cli-exe "run"
                   (append (if verbose-flag '("-v") '()) `("question" ,main-file))))
          (fprintf stdin "~a" stdin-contents)
          (close-output-port stdin)
          (define process-exit-code (subprocess-status (sync proc)))
          (values process-exit-code
                  (port->string stdout)
                  (port->string stderr)))
   (thunk (void))))

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

;; ------ "marmtest" Command Tests ----------------------------------------------------------------
    (test-case "[marmtest] A program fails to compile"
      (check-equal?
        (cli-marmtest "good code;\n" "" "")
        10))

    (test-case "[marmtest] A program crashes at runtime"
      (check-equal?
        (cli-marmtest "#include <stdio.h>\nint main(){int *p=NULL;printf(\"%d\\n\",*p);}"
                      "" "")
        20))

    (test-case "[marmtest] A program fails an assertion"
      (check-equal?
        (cli-marmtest "#include <assert.h>\nint main(){assert(0);}"
                      "" "")
        21))

    (test-case "[marmtest] A program fails the test"
      (check-equal?
        (cli-marmtest "#include <stdio.h>\nint main(){int x;scanf(\"%d\",&x);printf(\"%d\\n\",x);}"
                      "42\n" "43\n")
        30))

    (test-case "[marmtest] A program passes the test"
      (check-equal?
        (cli-marmtest "#include <stdio.h>\nint main(){int x;scanf(\"%d\",&x);printf(\"%d\\n\",x);}"
                      "42\n"
                      "42\n")
        40))

    (test-case "[marmtest] A program times out"
      (check-equal?
        (cli-marmtest "int main(){while(1){}}" "" "" 1)
        50))

;; ------ "run" Command Tests ----------------------------------------------------------------
    (test-case "[run] Compile and run a simple program"
      (define main-contents #<<HERE
#include <stdio.h>
int main() { printf("Hello World 1 2 3"); return 123; }
HERE
)
      (define-values (exit-code stdout stderr)
        (cli-run "main.c" `(("main.c" ,main-contents))))
      (check-equal? exit-code 0)
      (check-equal? stdout "Hello World 1 2 3")
      (check-true (string-contains? stderr "Program finished with exit code 123.")))

    (test-case "[run] Compile and run a program using common folder"
      (define main.c #<<HERE
#include <stdio.h>
#include "sqr.h"
#include "cube.h"
int main() {
    printf("sqr(5) = %d\n", sqr(5));
    printf("cube(5) = %d\n", cube(5));
    return 42;
}
HERE
)
      (define sqr.h "int sqr(int);")
      (define sqr.c "#include \"sqr.h\"\nint sqr(int x) { return x*x; }")

      (define cube.h "int cube(int);")
      (define cube.c "#include \"cube.h\"\nint cube(int x) { return x*x*x; }")

      (define-values (exit-code stdout stderr)
        (cli-run "main.c" `(("main.c" ,main.c) ("sqr.h" ,sqr.h) ("sqr.c" ,sqr.c))
                 `(("cube.c" ,cube.c) ("cube.h" ,cube.h))))
      (check-equal? exit-code 0)
      (check-equal? stdout "sqr(5) = 25\ncube(5) = 125\n")
      (check-true (string-contains? stderr "Program finished with exit code 42.")))

    (test-case "[run] Compile and run a program that reads from stdin"
      (define main.c #<<HERE
#include <stdio.h>
int main() {
    int x, y;
    scanf("%d %d", &x, &y);
    printf("%d + %d = %d\n", x, y, x + y);
    printf("%d - %d = %d\n", x, y, x - y);
    return 42;
}
HERE
)
      (define-values (exit-code stdout stderr)
        (cli-run "main.c" `(("main.c" ,main.c)) '() "10 3"))
      (check-equal? exit-code 0)
      (check-equal? stdout "10 + 3 = 13\n10 - 3 = 7\n")
      (check-true (string-contains? stderr "Program finished with exit code 42.")))

    (test-case "[run] Program fails to compile"
      (define main-contents "int main() return 9;")
      (define-values (exit-code stdout stderr) (cli-run "main.c" `(("main.c" ,main-contents))))
      (check-equal? exit-code 1)
      (check-equal? stdout "")
      (check-true (string-contains? stderr "Your code did not compile (clang exit code: 1).")))

    ;; TODO: update this test to work with the new backend
    ;(test-case "Generate a .ll file and run a project with it"
    ;  (new-project "beep")
    ;  (define q1-path (build-path (build-project-path "beep") "q1"))
    ;  (make-directory q1-path)
    ;  (check-equal?
    ;    (cli-object "int magic() { return 42; }\n" (path->string (build-path q1-path "magic")))
    ;    0)
    ;  (with-output-to-file (build-path q1-path "magic.h")
    ;    (thunk (display "int magic();\n")))
    ;  (with-output-to-file (build-path q1-path "main.c")
    ;    (thunk (display "#include <stdio.h>\n#include \"magic.h\"\nint main() { printf(\"%d\\n\", magic()); }\n")))
    ;  (define-values (suc res)
    ;    (compile-and-run-project "beep" "q1/main.c" "q1" '()))
    ;  (check-true suc)
    ;  (delete-project "beep"))
  ))
