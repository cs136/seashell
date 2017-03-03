#lang racket

(require seashell/backend/project
         seashell/backend/runner
         seashell/log
         seashell/compiler
         seashell/seashell-config
         racket/serialize
         racket/cmdline
         json)

(define (marmtest-main)
  (define RUN-TIMEOUT (make-parameter #f))
  (define-values (project-dir main-file test-name out-file err-file)
    (command-line
      #:program "seashell-cli marmtest"
      #:argv (rest flags)
      #:usage-help "Seashell command-line tester. Return codes:\n  10 means failed compilation.\n  20 means the program crashed at runtime.\n  21 means the program failed an assert.\n  30 means the program failed its test.\n  40 means the program passed its test."
      #:once-each
      [("-t" "--timeout") timeout
                          "Override the default seashell timeout (seconds)."
                          (RUN-TIMEOUT (string->number timeout))]
      #:args (project-dir main-file test-name out-file err-file)
      (values project-dir main-file test-name out-file err-file)))

  (when (RUN-TIMEOUT)
    (config-set! 'program-run-timeout (RUN-TIMEOUT)))

  (define temp-dir-path (make-temporary-file "seashell-runtime-~a" 'directory))
  (define default-exit-handler (exit-handler))
  (exit-handler (lambda (exit-code)
                  (delete-directory/files temp-dir-path #:must-exist? #f)
                  (default-exit-handler exit-code)))
  (config-set! 'runtime-files-path temp-dir-path)

  (define/contract (write-outputs stdout stderr asan)
    (-> (or/c bytes? #f) (or/c bytes? #f) (or/c bytes? #f) void?)
    (define plain-asan
      (if (not asan) #f
        (let ([parsed (bytes->jsexpr asan)])
          (if (string=? "" (hash-ref parsed 'raw_message))
            #f (hash-ref parsed 'raw_message)))))
    (when stdout
      (eprintf "Writing program stdout to ~s.~n" out-file)
      (with-output-to-file out-file (thunk
        (write-bytes stdout))
        #:exists 'truncate))
    (when (or stderr plain-asan)
      (eprintf "Writing program stderr and ASAN output to ~s.~n" err-file)
      (with-output-to-file err-file (thunk
        (when stderr (write-bytes stderr))
        (when plain-asan (display plain-asan)))
        #:exists 'truncate))
    (void))

  ;; nicely formats a compiler message to be output to the user
  (define/contract (format-message msg)
    (-> list? string?)
    (match-define (list error? file line column errstr) msg)
    (format "~a:~a:~a: ~a: ~a~n" file line column (if error? "error" "warning") errstr))

  (define-values (code info) (compile-and-run-project (path->complete-path project-dir) main-file "." (list test-name) #t 'current-directory))
  (match info
    [(hash-table ('messages msgs) ('status "compile-failed"))
      (eprintf "Compilation failed. Compiler errors:~n")
      (define compiler-errors (apply string-append (map format-message msgs)))
      (eprintf compiler-errors)
      (write-outputs #f (string->bytes/utf-8 compiler-errors) #f)
      (exit 10)]
    [(hash-table ('pids (list pid)) ('messages messages) ('status "running"))
      (eprintf "Waiting for program to finish...~n")
      (sync (program-wait-evt pid))

      ;; TODO: separate this block into its own function?
      (define stdout (program-stdout pid))
      (define run-result (sync/timeout 0 (wrap-evt stdout (compose deserialize read))))
      (match run-result
       [(and result (list pid _ (and test-res (or "timeout" "killed" "passed")) stdout stderr))
        (eprintf "Program passed the test.\n")
        (write-outputs stdout stderr #f)
        (exit 40)]
       [(list pid _ "error" exit-code stderr stdout asan)
        (if (= exit-code 134)
          (eprintf "Program failed an assertion.~n")
          (eprintf "Program crashed at runtime.~n"))
        (write-outputs #f stderr asan)
        (exit (if (= exit-code 134) 21 20))]
       [(list pid _ "no-expect" stdout stderr asan)
        (eprintf "No expect file for test; program did not crash.~n")
        (write-outputs stdout stderr asan)
        (exit 99)]
       [(list pid _ "failed" diff stderr stdout asan)
        (eprintf "Test failed the test (but did not crash)~n")
        (write-outputs stdout stderr asan)
        (exit 30)]
       [(list pid _ "timeout")
        (eprintf "Test timed out (but did not crash)~n")
        (exit 50)]
       [x
        (eprintf "Unknown error occurred: ~s~n" x)
        (exit 98)]
       )]
    [x (error (format "Seashell failed: compile-and-run-project returned ~s.~n" x))]))


(define (object-main)
  (define args
    (command-line
      #:program "seashell-cli object"
      #:argv (rest flags)
      #:usage-help "Generate object files (actually LLVM bytecode) for use with Seashell. Takes in a list of files to generate code for."
      #:args args
      args))
  (void (map (lambda (arg)
    (define-values (result errs) (seashell-generate-bytecode (path->complete-path (string->path arg))))
    (if result
      (let ([fparts (string-split arg ".")])
        (with-output-to-file (string-join (append (drop-right fparts 1) '("ll")) ".")
          (thunk (display result)) #:exists 'replace))
      (write errs))) args)))

(define tools `#hash(("marmtest" . (,marmtest-main "Marmoset test runner."))
                     ("object" . (,object-main "Generate object files for Seashell."))))

;; main program execution begins here
(define flags (vector->list (current-command-line-arguments)))

(when (or (empty? flags) (not (hash-has-key? tools (first flags))))
  (printf "seashell-cli <tool>; possible tools are:~n")
  (hash-for-each tools (lambda (tool props)
    (printf "  ~a: ~a~n" tool (second props))))
  (exit 1))

(standard-logger-setup)
;; invoke main method for tool
((first (hash-ref tools (first flags))))
