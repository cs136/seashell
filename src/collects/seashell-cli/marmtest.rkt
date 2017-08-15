#lang typed/racket

(require seashell/backend/runner
         seashell/backend/project
         seashell/compiler
         (submod seashell/seashell-config typed)
         racket/cmdline
         typed/json)

(require/typed racket/serialize
  [deserialize (-> Any Any)])

(provide marmtest-main)

;; nicely formats a compiler message to be output to the user
(: format-message (-> (List Boolean String Number Number String) String))
(define (format-message msg)
  (match-define (list error? file line column errstr) msg)
  (format "~a:~a:~a: ~a: ~a~n" file line column (if error? "error" "warning") errstr))

(: marmtest-main (-> (Listof String) Void))
(define (marmtest-main flags)
  (: RUN-TIMEOUT (Parameter (U False Number)))
  (define RUN-TIMEOUT (make-parameter #f))
  (match-define (list project-dir main-file test-name out-file err-file)
    (parse-command-line "seashell-cli marmtest" flags
      `((once-each
        [("-t" "--timeout")
          ,(lambda ([flag : String] [timeout : String]) (RUN-TIMEOUT (string->number timeout)))
          ("Override the default Seashell timeout (seconds)."
           "timeout")]))
      (lambda (flag-accum project-dir main-file
               test-name out-file err-file . args)
        (cast (list project-dir main-file test-name out-file err-file)
          (List String String String String String)))
      '("Directory of project being run"
        "File to run"
        "Name of test to run"
        "File to redirect stdout to"
        "File to redirect stderr to")))

  (when (RUN-TIMEOUT)
    (config-set! 'program-run-timeout (RUN-TIMEOUT)))

  (define temp-dir-path (make-temporary-file "seashell-runtime-~a" 'directory))
  (define default-exit-handler (exit-handler))
  (exit-handler (lambda (exit-code)
                  (delete-directory/files temp-dir-path #:must-exist? #f)
                  (default-exit-handler exit-code)))
  (config-set! 'runtime-files-path temp-dir-path)

  (: write-outputs (-> (U False Bytes) (U False Bytes) (U False Bytes) Void))
  (define (write-outputs stdout stderr asan)
    (define plain-asan
      (if (not asan) #f
        (let ([parsed (cast (bytes->jsexpr asan) (HashTable Symbol JSExpr))])
          (if (string=? "" (cast (hash-ref parsed 'raw_message) String))
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

  (define-values (code info)
    (compile-and-run-project (path->string (path->complete-path project-dir)) main-file "." (list test-name) 'current-directory))
  (match info
    [(hash-table ('messages msgs) ('status "compile-failed"))
      (eprintf "Compilation failed. Compiler errors:~n")
      (define compiler-errors (apply string-append (map format-message (cast msgs (Listof (List Boolean String Number Number String))))))
      (eprintf compiler-errors)
      (write-outputs #f (string->bytes/utf-8 compiler-errors) #f)
      (exit 10)]
    [(hash-table ('pids (list pid)) ('messages messages) ('status "running"))
      (eprintf "Waiting for program to finish...~n")
      (sync (program-wait-evt (cast pid Integer)))

      ;; TODO: separate this block into its own function?
      (define stdout (program-stdout (cast pid Integer)))
      (define run-result (cast (sync/timeout 0 (wrap-evt stdout (compose deserialize read))) (Listof Any)))
      (match run-result
       [(and result (list pid _ (and test-res (or "killed" "passed")) stdout stderr))
        (eprintf "Program passed the test.\n")
        (write-outputs (cast stdout Bytes) (cast stderr Bytes) #f)
        (exit 40)]
       [(list pid _ "error" exit-code stderr stdout asan)
        (if (= (cast exit-code Integer) 134)
          (eprintf "Program failed an assertion.~n")
          (eprintf "Program crashed at runtime.~n"))
        (write-outputs #f (cast stderr Bytes) (cast asan Bytes))
        (exit (if (= (cast exit-code Integer) 134) 21 20))]
       [(list pid _ "no-expect" stdout stderr asan)
        (eprintf "No expect file for test; program did not crash.~n")
        (write-outputs (cast stdout Bytes) (cast stderr Bytes) (cast asan Bytes))
        (exit 99)]
       [(list pid _ "failed" diff stderr stdout asan)
        (eprintf "Test failed the test (but did not crash)~n")
        (write-outputs (cast stdout Bytes) (cast stderr Bytes) (cast asan Bytes))
        (exit 30)]
       [(or (list pid _ "timeout") (list pid _ "timeout" _ _))
        (eprintf "Test timed out (but did not crash)~n")
        (exit 50)]
       [x
        (eprintf "Unknown error occurred: ~s~n" x)
        (exit 98)]
       )]
    [x (error (format "Seashell failed: compile-and-run-project returned ~s.~n" x))]))
