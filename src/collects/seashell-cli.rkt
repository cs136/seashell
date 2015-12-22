#lang racket

(require seashell/backend/project
         seashell/backend/runner
         seashell/log
         seashell/compiler
         racket/serialize
         racket/cmdline)


(define-values (project-dir main-file test-name out-file err-file)
  (command-line
    #:usage-help "Seashell command-line runner. Return codes:\n  10 means failed compilation\n  20 means the program crashed at runtime\n  30 means the program failed its test\n  40 means the program passed its test"
    #:args (project-dir main-file test-name out-file err-file)
    (values project-dir main-file test-name out-file err-file)))

(define/contract (write-outputs stdout stderr)
  (-> (or/c bytes? #f) (or/c bytes? #f) void?)
  (when stdout
    (eprintf "Writing program stdout to ~s\n" out-file)
    (with-output-to-file out-file (thunk
      (write-bytes stdout))
      #:exists 'truncate))
  (when stderr
    (eprintf "Writing program stderr to ~s\n" err-file)
    (with-output-to-file err-file (thunk
      (write-bytes stderr))
      #:exists 'truncate))
  (void))

(standard-logger-setup)
(seashell-compile-place/init)
(define-values (code info) (compile-and-run-project project-dir main-file (list test-name) #t))
(match info 
  [(hash-table ('messages msgs) ('status "compile-failed"))
    (eprintf "Compilation failed.\n")
    (exit 10)]
  [(hash-table ('pids (list pid)) ('messages messages) ('status "running"))
    (eprintf "Waiting for program to finish...\n")
    (sync (program-wait-evt pid))

    ;; TODO: separate this block into its own function?
    (define stdout (program-stdout pid))
    (match (sync (wrap-evt stdout (compose deserialize read)))
     [(and result (list pid _ (and test-res (or "timeout" "killed" "passed")) stdout stderr))
      (eprintf "Program passed the test.\n")
      (write-outputs stdout stderr)
      (exit 40)]
     [(list pid _ "error" exit-code stderr)
      (eprintf "Program crashed at runtime.\n")
      (write-outputs #f stderr)
      (exit 20)]
     [(list pid _ "no-expect" stdout stderr)
      (eprintf "No expect file for test; program did not crash.\n")
      (write-outputs stdout stderr)
      (exit 99)]
     [(list pid _ "failed" diff stderr stdout)
      (eprintf "Test failed the test (but did not crash)\n")
      (write-outputs stdout stderr)
      (exit 30)]
     [(list pid _ "timeout")
      (eprintf "Test timed out (but did not crash)\n")
      (exit 50)]
     [x
      (eprintf "Unknown error occurred: ~a" x)
      (exit 98)]
     )]
  [x (error (format "Seashell failed: compile-and-run-project returned ~s" x))])
