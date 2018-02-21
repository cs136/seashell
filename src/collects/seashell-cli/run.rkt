#lang typed/racket

(require seashell/backend/runner
         seashell/backend/project
         seashell/compiler
         (submod seashell/seashell-config typed)
         racket/cmdline
         seashell/compiler/ffi seashell/log
         racket/string
         typed/json)

(require/typed racket/serialize
               [deserialize (-> Any Any)])

(provide run-main)

(: run-main (-> (Listof String) Void))
(define (run-main flags)

  ;; Read command line arguments
  (: verbose-mode (Parameter Boolean))
  (define verbose-mode (make-parameter #f))
  (match-define (list project-dir main-file)
    (parse-command-line "seashell-cli run" flags
       `((once-each [("-v" "--verbose") ,(lambda (flag) (verbose-mode #t)) ("Compile with verbose messages")]))
       (lambda (flag-accum project-dir main-file)
          (cast (list project-dir main-file) (List String String)))
       '("Directory of project being run"
         "File to run")))

  (config-refresh!)

  ;; Run the Seashell custom clang/llvm wrapper so that we can get the dependency
  ;; paths. This is the only reason for running it.
  (define compiler (seashell_compiler_make))
  (seashell_compiler_clear_source_dirs compiler)
  (seashell_compiler_clear_compile_flags compiler)
  (seashell_compiler_set_main_file compiler (path->string (build-path project-dir main-file)))
  (seashell_compiler_add_source_dir compiler project-dir)
  (when (directory-exists? (build-path project-dir 'up "common"))
    (seashell_compiler_add_source_dir compiler (path->string (build-path project-dir 'up "common"))))
  (seashell_compiler_run compiler #f)

  ;; Output file path - student's binary executable will be saved here
  (define output-file-path (make-temporary-file "seashell-cli-result-~a"))
  (file-or-directory-permissions output-file-path #o700)

  ;; Path of output file
  (define default-exit-handler (exit-handler))
  (exit-handler (lambda (exit-code)
                  (delete-directory/files output-file-path #:must-exist? #f)
                  (default-exit-handler exit-code)))

  ;; Create clang command line path and arguments
  (define clang-binary-path (read-config-path 'clang-binary-path)) 
  (define clang-binary-arguments
            `("-std=c99"
              "-fsanitize=address" "-fno-omit-frame-pointer" "-fno-common"
              "-g" "-O0"
              ,(string-append "-I" project-dir)
              ,(string-append "-I" (path->string (build-path project-dir 'up "common")))
              "-lm"
              "-o" ,(path->string output-file-path)
              ,@(string-split (seashell_compiler_get_dep_paths compiler)))) 

  ;; Run the clang executable
  (cond [(verbose-mode) (logf 'info "Running clang compiler: ~a ~a" clang-binary-path (string-join clang-binary-arguments))]
        [else (logf 'info "Compiling your code...")])
  (define-values (clang-result clang-stdout clang-stdin clang-stderr)
    (apply subprocess (current-output-port) #f (current-error-port)
                      clang-binary-path
                      clang-binary-arguments))
  ;; Wait for compiler to finish
  (define clang-binary-exit-code (subprocess-status (sync clang-result)))
  (cond [(equal? clang-binary-exit-code 0)
         (when (verbose-mode) (logf 'info "Your code compiled."))]
        [else
         (logf 'info "Your code did not compile (clang exit code: ~a)." clang-binary-exit-code)
         (exit 1)])

  ;; Set file permissions on student's output binary and run it
  (file-or-directory-permissions output-file-path #o700)
  (putenv "ASAN_OPTIONS" "allocator_may_return_null=1:detect_leaks=1:detect_stack_use_after_return=1")
  (putenv "ASAN_SYMBOLIZER_PATH" (some-system-path->string (build-path (read-config-path 'llvm-symbolizer))))
  (with-handlers ([exn:break? (lambda (x) (exit 130))])
      (logf 'info "Running your program...")
      (define-values (output-result output-stdout output-stdin output-stderr)
        (subprocess (current-output-port) (current-input-port) (current-output-port) output-file-path))
      (define output-file-exit-code (subprocess-status (sync output-result)))
      (logf 'info "Program finished with exit code ~a." output-file-exit-code)
      (exit 0)))
