#lang racket

(require seashell/backend/project
         seashell/backend/offline
         rackunit
         json)

(define/contract (create-changes projects files changes settings)
  (-> (listof (list/c string? integer? (or/c string? #f)))
      (listof (list/c string? string? (or/c string? #f)))
      (listof (list/c string? (list/c string? string?) (or/c string? #f) (or/c string? #f) (or/c string? #f)))
      (list/c integer? string?)
      jsexpr?)
  `#hasheq((projects . ,(map (lambda (p) `#hasheq((name . ,(first p)) (last_modified . ,(second p)) (settings . ,(third p)))) projects))
           (files . ,(map (lambda (f) `#hasheq((project . ,(first f)) (file . ,(second f)) (checksum . ,(third f)))) files))
           (changes . ,(map (lambda (c) `#hasheq((type . ,(first c)) (file . #hasheq((project . ,(first (second c))) (file . ,(second (second c))) (checksum . #f))) (contents . ,(third c)) (checksum . ,(fourth c)) (history . ,(fifth c)))) changes))
           (settings . #hasheq((modified . ,(first settings)) (values . ,(second settings))))))

(define/provide-test-suite offline-suite
  #:before (thunk
    ;; start this test with a clean slate
    (delete-directory/files (project-base-path))
    (make-directory (project-base-path))
    (make-directory (build-project-path "beep")))
  #:after (thunk (delete-project "beep"))
  (test-suite "offline mode/syncing test suite"
    (test-case "An initial sync of a new project"
      (define res (sync-offline-changes (create-changes '() '() '() '(0 "{}"))))
      (check-equal? (first (hash-ref res 'newProjects)) "beep")
      (check-equal? (length (hash-ref res 'changes)) 0)
      (check-equal? (length (hash-ref res 'conflicts)) 0))

    (test-case "New file in the offline changes"
      (define res (sync-offline-changes (create-changes '(("beep" 0 #f)) '(("beep" "q1/main.c" #f)) '(("newFile" ("beep" "q1/main.c") "int main() { }\n" #f #f)) '(0 "{}"))))
      (fprintf (current-error-port) "~a~n" res)
      (check-true (file-exists? (build-path (build-project-path "beep") "q1/main.c")))
      (check-equal? (length (hash-ref res 'conflicts)) 0)
      (check-equal? (length (hash-ref res 'changes)) 1))

    (test-case "Delete a file on the backend and sync"
      (delete-file (build-path (build-project-path "beep") "q1/main.c"))
      (define res (sync-offline-changes (create-changes '(("beep" 0 #f)) '(("beep" "q1/main.c" #f)) '() '(0 "{}"))))
      (check-equal? (length (hash-ref res 'conflicts)) 0)
      (check-equal? (length (hash-ref res 'changes)) 1))
  ))
