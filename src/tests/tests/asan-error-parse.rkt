#lang racket

(require rackunit
         racket/serialize
         seashell/backend/asan-error-parse
         seashell/backend/project
         seashell/backend/runner
         seashell/seashell-config
         json)

;; Convenience function for creating a project, compiling,
;; running, and waiting for it to finish. Returns the ASAN
;; output as a JSON.
(define (compile-run-wait code [project-name (symbol->string (gensym 'project))])
  (new-project-from project-name (format "file://~a/src/tests/template.zip" SEASHELL_SOURCE_PATH))
  (with-output-to-file (check-and-build-path (build-project-path project-name) "default" "main.c")
    (thunk (display code)) #:exists 'replace)
  (define-values (success hsh) (compile-and-run-project project-name "default/main.c" "default" '()))
  (sync (program-wait-evt (hash-ref hsh 'pid)))
  (string->jsexpr (bytes->string/utf-8 (program-asan-message (hash-ref hsh 'pid)))))

;; Checks the ASAN JSON result has the specified type
;; and the specified number of "stacks". If a number
;; of stacks is not specified, checks that there is at
;; least one.
(define (has-type-and-stack? result type [stacks #f])
  (and (string=? (hash-ref result 'error_type) type)
       (if stacks
           (= stacks (length (hash-ref result 'call_stacks)))
           (not (empty? (hash-ref result 'call_stacks))))))

(define/provide-test-suite asan-parser-suite
  (test-suite "ASAN Parser Tests"

;; ---- MEMORY LEAK TESTS ---------------------------
    (test-case "Memory Leak Test 1"
      (define json-answer (compile-run-wait "#include <stdlib.h>\n\nint main() {\nmalloc(100);\nreturn 0; }"))
      (check-equal? (hash-ref json-answer 'error_type) "memory-leak")
      (check-equal? (length (hash-ref json-answer 'call_stacks)) 1))

    (test-case "Memory Leak Test 2"
      (define student-code #<<HERE
#include <stdlib.h>
int *f(int x) { return malloc(x); }
int main() {
    f(123);
    int *p = f(456);
    f(789);
    return p[3];
}
HERE
)
      (define json-answer (compile-run-wait student-code))
      (check-true (has-type-and-stack? json-answer "memory-leak" 3)))

;; ---- STACK OVERFLOW TESTS ---------------------------
    (test-case "Stack Overflow Test 1"
      (define student-code #<<HERE
int main() {
    int my_var[20];
    return my_var[20];
}
HERE
)
      (define json-answer (compile-run-wait student-code))
      (check-true (has-type-and-stack? json-answer "stack-buffer-overflow"))
      (check-equal? (hash-ref (hash-ref json-answer 'misc) 'array_my_var_size_in_bytes) "80"))

    (test-case "Stack Overflow Test 2"
      (define student-code #<<HERE
void g(int x) { char carr[30]; carr[x] = 111; }
int main() {
    g(40);
    return 0;
}
HERE
)
      (define json-answer (compile-run-wait student-code))
      (check-true (has-type-and-stack? json-answer "stack-buffer-overflow"))
      (check-equal? (hash-ref (hash-ref json-answer 'misc) 'array_carr_size_in_bytes) "30"))

    ;; This next test really should be an underflow, but ASAN for some reason thinks that
    ;; we're overflowing the parameter x (and it doesn't report the variable name).
    ;; Fixed in Clang 3.9
    (test-case "Stack Underflow Test 3"
      (define student-code #<<HERE
void g(int x) { char carr[30]; carr[x] = 111; }
int main() {
    g(-10);
    return 0;
}
HERE
)
      (define json-answer (compile-run-wait student-code))
      (check-true (has-type-and-stack? json-answer "stack-buffer-underflow")))

    (test-case "Stack Overflow Test 4"
      (define student-code #<<HERE
#include <string.h>

struct Employee {
  char *name;
  char *initials;
};

int main() {
  char name[100], initials[2];
  struct Employee emp = {name, initials};
  for (int i = 0; i < 100; ++i) { emp.name[i] = 'X'; }
  return strlen(emp.name);
}
HERE
)
      (define json-answer (compile-run-wait student-code))
      (check-true (has-type-and-stack? json-answer "stack-buffer-overflow"))
      (check-true (hash-has-key? (hash-ref json-answer 'misc) 'array_emp_size_in_bytes))
      (check-true (hash-has-key? (hash-ref json-answer 'misc) 'underflow_distance_in_bytes_from_start_of_array_emp))
      (check-true (hash-has-key? (hash-ref json-answer 'misc) 'array_initials_size_in_bytes))
      (check-true (hash-has-key? (hash-ref json-answer 'misc) 'underflow_distance_in_bytes_from_start_of_array_initials)))

;; ---- STACK UNDERFLOW TESTS ---------------------------
    (test-case "Stack Underflow Test 1"
      (define student-code #<<HERE
int main() {
    int x[20];
    return x[-1];
}
HERE
)
      (define json-answer (compile-run-wait student-code))
      (check-true (has-type-and-stack? json-answer "stack-buffer-underflow"))
      (check-equal? (hash-ref (hash-ref json-answer 'misc) 'array_x_size_in_bytes) "80"))

;; ---- STACK USE AFTER RETURN TEST ---------------------------
    (test-case "Stack Use After Return"
      (define student-code #<<HERE
int *f() {
    int x;
    return &x;
}
int main() {
    int *p = f();
    *p = 99999;
    return 0;
}
HERE
)
      (define json-answer (compile-run-wait student-code))
      (check-true (has-type-and-stack? json-answer "stack-use-after-return")))

;; ---- HEAP OVERFLOW TESTS ---------------------------
    (test-case "Heap Overflow Test 1"
      (define student-code #<<HERE
int main() {
    int *x = malloc(10 * sizeof(*x));
    int r = x[12];
    free(x);
    return 0;
}
HERE
)
      (define json-answer (compile-run-wait student-code))
      (check-true (has-type-and-stack? json-answer "heap-buffer-overflow")))

    (test-case "Heap Overflow Test 2"
      (define student-code #<<HERE
void h(int *p) { p[150] = 99999; free(p); }
int main() {
    h(malloc(100));
    return 0;
}
HERE
)
      (define json-answer (compile-run-wait student-code))
      (check-true (has-type-and-stack? json-answer "heap-buffer-overflow")))

;; ---- DOUBLE FREE TEST ---------------------------
    (test-case "Double Free Test"
      (define student-code #<<HERE
void *get100bytes() {
    void *p = malloc(100);
    free(p);
    return p;
}
int main() {
    int *p = get100bytes();
    free(p);
    return 0;
}
HERE
)
      (define json-answer (compile-run-wait student-code))
      (check-true (has-type-and-stack? json-answer "double-free" 3)))

;; ---- HEAP USE AFTER FREE TEST ---------------------------
    (test-case "Heap Use After Free Test"
      (define student-code #<<HERE
void *get100bytes() { return malloc(100); }
int main() {
    int *p = get100bytes();
    free(p);
    p[10] = 999;
    return 0;
}
HERE
)
      (define json-answer (compile-run-wait student-code))
      (check-true (has-type-and-stack? json-answer "heap-use-after-free" 3)))


;; ---- GLOBAL MEMORY TEST ---------------------------
    (test-case "Global Memory Test"
      (define student-code #<<HERE
int my_array[100];
int main() {
    my_array[120] = 9999;
    return 0;
}
HERE
)
      (define json-answer (compile-run-wait student-code))
      (check-true (has-type-and-stack? json-answer "global-buffer-overflow")))

;; ---- SEGMENTATION FAULT TESTS ---------------------------
    (test-case "Segmentation Fault Null Test"
      (define student-code #<<HERE
void f(char *x) { *x = 'a'; }
int main() {
    f(0);
    return 0;
}
HERE
)
      (define json-answer (compile-run-wait student-code))
      (check-true (has-type-and-stack? json-answer "segmentation-fault-on-null-address")))

    (test-case "Segmentation Fault Test"
      (define student-code #<<HERE
int main() {
    int *p = (int*) main;
    while(1) { *p++ = 0xdeadbeef; }
    return 0;
}
HERE
)
      (define json-answer (compile-run-wait student-code))
      (check-true (has-type-and-stack? json-answer "segmentation-fault")))

;; ---- NO ERROR TEST ---------------------------
    (test-case "No Error Test"
      (define student-code #<<HERE
#include <stdlib.h>

int global[10];

void f(char *a, int len) {
  for(int i = 0; i < len; i++) {
    a[i] = 'x';
  }
}

int main() {
  double *p = malloc(100 * sizeof(p));
  for(int i = 0; i < 100; i++) {
    p[i] = 0.123 * i;
  }
  free(p);
  p = NULL;
  return 0;
}
HERE
)
      (define json-answer (compile-run-wait student-code))
      (check-true (has-type-and-stack? json-answer "unknown" 0))
      (check-equal? (hash-ref json-answer 'raw_message) ""))

))
