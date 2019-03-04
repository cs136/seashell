#lang typed/racket
;; Seashell
;; Copyright (C) 2012-2014 The Seashell Maintainers
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; See also 'ADDITIONAL TERMS' at the end of the included LICENSE file.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
(require seashell/log
         (submod seashell/seashell-config typed)
         seashell/diff
         seashell/utils/pty)
(require/typed racket/serialize
               [serialize (-> Any Any)])
(require/typed racket/base
               [environment-variables-copy (-> Environment-Variables Environment-Variables)])
(require/typed "asan-error-parse.rkt"
               [asan->json (-> Bytes Path-String Bytes)])

(provide run-program program-stdin program-stdout program-stderr
         program-wait-evt program-kill program-status program-destroy-handle
         program-mode
         program-asan-message)

;; Global definitions and concurrency control primitives.
(struct program ([in-stdin : Input-Port] [in-stdout : Input-Port] [in-stderr : Input-Port]
                 [out-stdin : Output-Port] [out-stdout : Output-Port] [out-stderr : Output-Port]
                 [raw-stdin : Output-Port] [raw-stdout : Input-Port] [raw-stderr : Input-Port]
                 [handle : Subprocess] [control : (U False Thread)] [exit-status : (U False Exact-Nonnegative-Integer)]
                 [destroyed-semaphore : Semaphore]
                 [_mode : (U 'test 'run)] [custodian : Custodian]
                 [source-dir : Path-String]
                 [asan : Bytes]
                 [pty : (U False PTY)]) #:transparent #:mutable #:type-name Program)
(struct exn:program:run exn:fail:user ())

(: program-table (HashTable Integer Program))
(define program-table (make-hash))
(define program-new-semaphore (make-semaphore 1))
(define program-destroy-semaphore (make-semaphore 1))

;; (program-control-test-thread program)
;; Control thread helper for running a program with a test file.
;;
;; Arguments:
;;  program - Program structure.
;;  test-name - Name of test.
;;  input - input of test.
;;  expected - expected output.
;; Returns:
;;  Nothing.
(: program-control-test-thread (-> Program String Bytes (U False Bytes) Void))
(define (program-control-test-thread pgrm test-name input expected)
  (match-define (program in-stdin in-stdout in-stderr
                         out-stdin out-stdout out-stderr
                         raw-stdin raw-stdout raw-stderr
                         handle control exit-status
                         destroyed-semaphore mode custodian
                         source-dir asan pty)
                pgrm)
  (define pid (subprocess-pid handle))
  ;; Close ports we don't use.
  (close-input-port in-stdin)
  (close-output-port out-stderr)

  (logf 'debug "Sending ~a bytes to program PID ~a." (bytes-length input) pid)

  ;; Send test input to program and wait.
  (thread (lambda ()
            (with-handlers
             [(exn:fail?
               (lambda ([exn : exn])
                 (logf 'info "write-bytes failed to write ~a.in to program stdin: received error: ~a"
                       test-name (exn-message exn))))]
             (write-bytes input raw-stdin))
            (close-output-port raw-stdin)))

  ;; Background read stuff.
  (define-values (buf-stderr cp-stderr) (make-pipe (* (read-config-nonnegative-real 'subprocess-buffer-size) 1000000)))
  (define-values (buf-stdout cp-stdout) (make-pipe (* (read-config-nonnegative-real 'subprocess-buffer-size) 1000000)))
  (define stderr-thread (thread (lambda ()
                                  (logf 'debug "Starting copy port for stderr for program PID ~a." pid)
                                  (copy-port raw-stderr cp-stderr))))
  (define stdout-thread (thread (lambda ()
                                  (logf 'debug "Starting copy port for stdout for program PID ~a." pid)
                                  (copy-port raw-stdout cp-stdout))))

  ;; Helper to close ports (note -- there is no PTY when running a test)
  (define (close)
    (close-input-port raw-stdout)
    (close-input-port raw-stderr)
    (close-output-port out-stdout)
    (custodian-shutdown-all custodian)
    (void))

  ;; This helper function reads and returns the first n bytes of an input port.
  ;; Used when students write a ton of output and we don't want to send all of it to the front-end.
  (: first-n-bytes (->* (Input-Port Nonnegative-Real) (Bytes) Bytes))
  (define (first-n-bytes input-port n [too-long-message #"\n... There is more output, but it's not shown ...\n"])
    (local [(define first-part (read-bytes (exact-floor n) input-port))
            (define reached-end? (or (not (byte-ready? input-port)) (eof-object? (read-byte input-port))))]
      (cond [(eof-object? first-part) #""]
            [reached-end? first-part]
            [else (bytes-append first-part too-long-message)])))


  (define receive-evt (thread-receive-evt))
  (let loop ()
    (match (sync/timeout (read-config-nonnegative-real 'program-run-timeout)
                         #{handle :: (Evtof Any)}
                         #{receive-evt :: (Evtof Any)})
           [(? (lambda ([evt : Any]) (eq? handle evt))) ;; Program quit
            (logf 'info "Program with PID ~a quit with status ~a." pid (subprocess-status handle))
            (set-program-exit-status! pgrm (cast (subprocess-status handle) Exact-Nonnegative-Integer))
            ;; Wait for threads to finish copying
            (logf 'info "Waiting for threads of PID ~a to finish copying." pid)
            (thread-wait stderr-thread)
            (thread-wait stdout-thread)
            ;; Read stdout, stderr.
            (logf 'info "Closing ports for program with PID ~a." pid)
            (close-output-port cp-stderr)
            (close-output-port cp-stdout)
            ;; Read asan error message and convert it to json (stored as Bytes)
            (logf 'info "Parsing ASAN messages for program with PID ~a." pid)
            (set-program-asan! pgrm (asan->json (delete-read-asan pid) source-dir))

            (logf 'info "Converting port to bytes for program with PID ~a." pid)
            (define stdout (port->bytes buf-stdout))
            (define stderr (port->bytes buf-stderr))
            (define asan-output (program-asan pgrm))

            (logf 'info "Writing test results to out-stdout for program with PID ~a." pid)
            (match (subprocess-status handle)
                   [0
                    ;; Three cases:
                    (cond
                     [(not expected)
                      (write (serialize `(,pid ,test-name "no-expect" ,stdout ,stderr ,asan-output)) out-stdout)]
                     [(equal? stdout expected)
                      (write (serialize `(,pid ,test-name "passed" ,stdout ,stderr)) out-stdout)]
                     [else
                      ;; Split expected and output, difference them.
                      (define output-lines (regexp-split #rx"\n" stdout))
                      (define expected-lines (regexp-split #rx"\n" expected))
                      ;; hotfix: diff with empty because it's slow
                      (write (serialize `(,pid ,test-name "failed" ,(list-diff expected-lines '()) ,stderr ,stdout ,asan-output)) out-stdout)])]
                   [_ (write (serialize `(,pid ,test-name "error" ,(subprocess-status handle) ,stderr ,stdout ,asan-output)) out-stdout)])
            (logf 'info "Done sending test results for program PID ~a." pid)
            (close)]
           [#f ;; Program timed out ('program-run-timeout seconds pass without any event)
            (logf 'info "Program with PID ~a timed out after ~a seconds." pid (read-config-nonnegative-real 'program-run-timeout))
            (set-program-exit-status! pgrm 255)
            ;; Kill copy-threads before killing program
            (kill-thread stderr-thread)
            (kill-thread stdout-thread)
            (subprocess-kill handle #t)

            ;; Read stdout, stderr.
            (close-output-port cp-stderr)
            (close-output-port cp-stdout)
            (define stdout (first-n-bytes buf-stdout (read-config-nonnegative-real 'max-output-bytes-to-keep)))
            (define stderr (first-n-bytes buf-stderr (read-config-nonnegative-real 'max-output-bytes-to-keep)))

            (write (serialize `(,pid ,test-name "timeout" ,stdout ,stderr)) out-stdout)
            (close)]
           [(? (lambda ([evt : Any]) (eq? receive-evt evt))) ;; Received a signal.
            (match (thread-receive)
                   ['kill
                    (logf 'info "Program with PID ~a killed." pid)
                    (kill-thread stderr-thread)
                    (kill-thread stdout-thread)
                    (set-program-exit-status! pgrm 254)
                    (subprocess-kill handle #t)

                    ;; Read stdout, stderr.
                    (close-output-port cp-stderr)
                    (close-output-port cp-stdout)
                    (define stdout (first-n-bytes buf-stdout (read-config-nonnegative-real 'max-output-bytes-to-keep)))
                    (define stderr (first-n-bytes buf-stderr (read-config-nonnegative-real 'max-output-bytes-to-keep)))

                    (write (serialize `(,pid ,test-name "killed" ,stdout ,stderr)) out-stdout)
                    (close)])]))
  (void))

;; (drain-port in out)
;; Drains all available bytes from in to out.
(: drain-port (-> Input-Port Output-Port Any))
(define (drain-port in out)
  (let loop ()
    (define output (make-bytes (read-config-integer 'io-buffer-size)))
    (define read (read-bytes-avail!* output in))
    (when (and (number? read) (read . > . 0))
      (write-bytes output out 0 read)
      (loop))))


;; (program-control-thread program)
;; Control thread helper for a running program.
;;
;; Arguments:
;;  program - Program structure.
;; Returns:
;;  Nothing.
(: program-control-thread (-> Program Void))
(define (program-control-thread pgrm)
  (match-define (program in-stdin in-stdout in-stderr
                         out-stdin out-stdout out-stderr
                         raw-stdin raw-stdout raw-stderr
                         handle control exit-status
                         destroyed-semaphore mode custodian
                         source-dir asan pty)
                pgrm)
  (define pid (subprocess-pid handle))
  (define (close)
    (unless (port-closed? raw-stdout)
      (close-input-port raw-stdout))
    (unless (port-closed? raw-stderr)
      (close-input-port raw-stderr))
    (unless (port-closed? in-stdin)
      (close-input-port in-stdin))
    (unless (port-closed? raw-stdin)
      (close-output-port raw-stdin))
    ;; These two ports must be closed, despite being pipes.
    ;; We depend on EOF being sent.
    (unless (port-closed? out-stderr)
      (close-output-port out-stderr))
    (unless (port-closed? out-stdout)
      (close-output-port out-stdout))
    (assert (PTY? pty))
    (close-pty pty)
    (custodian-shutdown-all custodian)
    (void))

  ;; Deal with receiving signals,
  ;; in all cases. (so I/O does not block a kill signal)
  (: check-signals (-> (-> Any) Any))
  (define (check-signals tail)
    (if (sync/timeout 0 (thread-receive-evt))
        (match (thread-receive)
               ['kill
                (logf 'info "Program with PID ~a killed." pid)
                (set-program-exit-status! pgrm 254)
                (subprocess-kill handle #t)
                (close)])
      (tail)))

  (let loop : Any ()
    (define receive-evt (thread-receive-evt))
    (match (sync/timeout (read-config-nonnegative-real 'program-run-timeout)
                         handle
                         receive-evt
                         (if (port-closed? in-stdin)
                             never-evt
                           in-stdin)
                         (if (port-closed? raw-stderr)
                             never-evt
                           raw-stderr)
                         (if (port-closed? raw-stdout)
                             never-evt
                           raw-stdout))
           [(? (lambda (evt) (eq? handle evt))) ;; Program quit
            (logf 'info "Program with PID ~a quit with status ~a." pid (subprocess-status handle))
            (set-program-exit-status! pgrm (cast (subprocess-status handle) Exact-Nonnegative-Integer))
            ;; Flush the ports!
            (unless (port-closed? raw-stdout) (drain-port raw-stdout out-stdout))
            (unless (port-closed? raw-stderr) (drain-port raw-stderr out-stderr))
            (set-program-asan! pgrm (asan->json (delete-read-asan pid) source-dir))
            (close)]
           [#f ;; Program timed out (30 seconds pass without any event)
            (logf 'info "Program with PID ~a timed out." pid)
            (set-program-exit-status! pgrm 255)
            (subprocess-kill handle #t)
            (close)]
           [(? (lambda (evt) (eq? receive-evt evt))) ;; Received a signal.
            (check-signals loop)]
           [(? (lambda (evt) (eq? in-stdin evt))) ;; Received input from user
            (define input (make-bytes (read-config-integer 'io-buffer-size)))
            (define read (read-bytes-avail! input in-stdin))
            (when (integer? read)
              (write-bytes input raw-stdin 0 read))
            (when (eof-object? read)
              (close-input-port in-stdin)
              ;; Send <eof> (#x4) to the PTY, instead of closing the port.
              (write-byte #x4 raw-stdin))
            (check-signals loop)]
           [(? (lambda (evt) (eq? raw-stdout evt))) ;; Received output from program
            (define output (make-bytes (read-config-integer 'io-buffer-size)))
            (define read (read-bytes-avail! output raw-stdout))
            (when (integer? read)
              (write-bytes output out-stdout 0 read))
            (when (eof-object? read)
              (close-input-port raw-stdout)
              (close-output-port out-stdout))
            (check-signals loop)]
           [(? (lambda (evt) (eq? raw-stderr evt))) ;; Received standard error from program
            (define output (make-bytes (read-config-integer 'io-buffer-size)))
            (define read (read-bytes-avail! output raw-stderr))
            (when (integer? read)
              (write-bytes output out-stderr 0 read))
            (when (eof-object? read)
              (close-input-port raw-stderr)
              (close-output-port out-stderr))
            (check-signals loop)]))
  (void))

;; (run-program binary directory lang test [test-loc 'tree])
;;  Runs a program.
;;
;; Arguments:
;;   program   - the filename of the program to be run
;;   directory - the directory containing the program
;;   soruce-dir- the directory containing the program's source files
;;   lang      - the language with which to run the program ('C or 'racket)
;;   test      - if #f, I/O is done directly with the user. Otherwise, test is
;;               a string representing the name of a test to use in tests/.
;;               i.e. if test is "foo", input is from tests/foo.in, output is
;;               diffed against tests/foo.expect
;;   test-loc  - one of:
;;                'current-directory - Look for the test in the current directory.
;;                'flat - Look for the test in <directory>.
;;                'tree - Look for the test in <directory>/<tests-subdirectory>.
;;                Path-String - Look for the test in <test-loc>
;;               By default it is 'tree.
;;
;; Returns:
;;  PID - handle representing the run.  On a test, test results are written
;;    to stdout.  On an interactive run, data is forwarded to/from
;;    the program.
(: run-program (->* (Path-String Path-String Path-String (U 'C 'racket) (U False String))
                    ((U Path-String 'current-directory 'flat 'tree)) Integer))
(define (run-program binary directory source-dir lang test [test-loc 'tree])
  (define test-path (cond
                     [(eq? test-loc 'current-directory) (build-path ".")]
                     [(eq? test-loc 'flat) (build-path directory)]
                     [(eq? test-loc 'tree) (build-path directory (read-config-string 'tests-subdirectory))]
                     [(path-string? test-loc) test-loc]
                     [else (error "...unreachable.")]))
  (define run-custodian (make-custodian))
  (parameterize ([current-custodian run-custodian]
                 [current-subprocess-custodian-mode 'interrupt])
                (call-with-semaphore
                 program-new-semaphore
                 (lambda ()
                   (with-handlers
                    [(exn:fail:filesystem?
                      (lambda ([exn : exn])
                        (raise (exn:program:run
                                (format "Could not run binary: Received filesystem error: ~a" (exn-message exn))
                                (current-continuation-marks)))))]
                    (if test
                        (logf 'info "Running file ~a with language ~a using test ~a" binary lang test)
                      (logf 'info "Running file ~a with language ~a" binary lang))
                    (define-values (handle pty raw-stdout raw-stdin raw-stderr)
                      (parameterize
                       ([current-directory directory]
                        [current-environment-variables (environment-variables-copy (current-environment-variables))])
                       (define-values (pty slave:stdout slave:stdin master:stdout master:stdin)
                         (if (not test)
                             (let* ([pty (make-pty)]
                                    [slave:stdout (PTY-s-out pty)]
                                    [slave:stdin (PTY-s-in pty)]
                                    [master:stdout (PTY-m-in pty)]
                                    [master:stdin (PTY-m-out pty)])
                               (values pty slave:stdout slave:stdin master:stdout master:stdin))
                           (values #f #f #f #f #f)))
                       (define-values (handle r:stdout r:stdin r:stderr)
                         (match lang
                                ['C
                                 ;; Behaviour is inconsistent if we just exec directly.
                                 ;; This seems to work.  (why: who knows?)
                                 (putenv "ASAN_OPTIONS"
                                         (format "allocator_may_return_null=1:detect_leaks=1:log_path='/tmp/seashell-asan-~a':detect_stack_use_after_return=1"
                                                 (ormap (lambda (x) x) (list (getenv "USER") (getenv "LOGNAME") ""))))
                                 (putenv "ASAN_SYMBOLIZER_PATH" (some-system-path->string (build-path (read-config-path 'llvm-symbolizer))))
                                 (logf 'info (format "Running with env ASAN_OPTIONS=~a" (getenv "ASAN_OPTIONS")))
                                 (logf 'info (format "Running with env ASAN_SYMBOLIZER_PATH=~a" (getenv "ASAN_SYMBOLIZER_PATH")))
                                 (subprocess slave:stdout slave:stdin #f binary)]
                                ['racket (subprocess slave:stdout slave:stdin #f
                                                     (read-config-path 'racket-interpreter)
                                                     "-t"
                                                     (some-system-path->string (build-path (read-config-path 'seashell-racket-runtime-library)))
                                                     "-u" binary)]))
                       ;; Keep the slave end of the PTY open, so the I/O code doesn't crash on us.
                       ;; (It's automatically closed when the program terminates)
                       ;; NOTE: this does not work on FreeBSD -- https://stackoverflow.com/questions/23458160/final-output-on-slave-pty-is-lost-if-it-was-not-closed-in-parent-why
                       (values handle pty
                               (assert (or master:stdout r:stdout))
                               (assert (or master:stdin r:stdin))
                               (assert r:stderr))))
                    ;; Construct the I/O ports.
                    (define (make-io-pipe)
                      (if test (make-pipe) (make-pipe (read-config-integer 'io-buffer-size))))
                    (define-values (in-stdout out-stdout) (make-io-pipe))
                    (define-values (in-stdin out-stdin) (make-io-pipe))
                    (define-values (in-stderr out-stderr) (make-io-pipe))
                    ;; Set buffering modes
                    (file-stream-buffer-mode raw-stdin 'none)
                    ;; Construct the destroyed-semaphore
                    (define destroyed-semaphore (make-semaphore 0))
                    ;; Construct the control structure.
                    (define result (program in-stdin in-stdout in-stderr
                                            out-stdin out-stdout out-stderr
                                            raw-stdin raw-stdout raw-stderr
                                            handle #f #f destroyed-semaphore
                                            (if test 'test 'run) run-custodian
                                            source-dir #"" pty))
                    (define control-thread
                      (thread
                       (lambda ()
                         (if test
                             (program-control-test-thread result
                                                          test
                                                          (file->bytes (build-path test-path (string-append test ".in")))
                                                          (with-handlers
                                                           ([exn:fail:filesystem? (lambda (exn) #f)])
                                                           (file->bytes (build-path test-path (string-append test ".expect")))))
                           (program-control-thread result)))))
                    (set-program-control! result control-thread)
                    ;; Install it in the hash-table
                    (define pid (subprocess-pid handle))
                    ;; Block if there's still a PID in there.  This is to prevent
                    ;; nasty race-conditions in which a process which is started
                    ;; has the same PID as a process which is just about to be destroyed.
                    (define block-semaphore
                      (call-with-semaphore
                       program-destroy-semaphore
                       (lambda ()
                         (define handle (hash-ref program-table pid #f))
                         (if handle (program-destroyed-semaphore handle) #f))))
                    (when block-semaphore (sync (semaphore-peek-evt block-semaphore)))
                    (hash-set! program-table pid result)
                    (logf 'info "Binary ~a running as PID ~a." binary pid)
                    pid)))))

;; (program-stdin pid)
;; Returns the standard input port for a program
;;
;; Arguments:
;;  pid - PID of program.
;; Returns:
;;  Port to write to program's standard input.
(: program-stdin (-> Nonnegative-Integer Output-Port))
(define (program-stdin pid)
  (program-out-stdin (hash-ref program-table pid)))

;; (program-mode pid)
;; Returns if the program is running in regular or test mode.
;;
;; Arguments:
;;  pid - PID of program.
;; Returns:
;;  'test if running in test mode.
;;  'run if in regular mode.
(: program-mode (-> Integer (U 'test 'run)))
(define (program-mode pid)
  (program-_mode (hash-ref program-table pid)))

;; (program-stdout pid)
;; Returns the standard output port for a program.
;;
;; Arguments:
;;  pid - PID of program.
;; Returns:
;;  Port to read from program's standard output.
(: program-stdout (-> Integer Input-Port))
(define (program-stdout pid)
  (program-in-stdout (hash-ref program-table pid)))

;; (program-stderr pid)
;; Returns the standard error port for a program.
;;
;; Arguments:
;;  pid - PID of program.
;; Returns:
;;  Port to read from program's standard error.
(: program-stderr (-> Integer Input-Port))
(define (program-stderr pid)
  (program-in-stderr (hash-ref program-table pid)))

;; (program-kill pid)
;; Kills a program
;;
;; Aguments:
;;  pid - PID of program.
;; Returns:
;;  Nothing
(: program-kill (-> Integer Void))
(define (program-kill pid)
  (thread-send (program-wait-evt pid)
               'kill
               (lambda ()
                  (define handle (program-handle (hash-ref program-table pid)))
                  (logf 'error "program-kill thread-send failed. Pid: ~a, exit code: ~a" pid (subprocess-status handle))
                  #f))
  (void))

;; (program-wait-evt pid)
;; Gets the event that will be ready when the program is done.
;;
;; Arguments:
;;  pid - PID of program.
;; Returns:
;;  An event that is ready when the program is done.
(: program-wait-evt (-> Integer Thread))
(define (program-wait-evt pid)
  (define result (program-control (hash-ref program-table pid)))
  (assert result)
  result)

;; (program-status pid)
;; Returns the exit status of the program.
;;
;; Arguments:
;;  pid - PID of program.
;; Returns:
;;  #f if program is not done, exit status otherwise
(: program-status (-> Integer (U False Integer)))
(define (program-status pid)
  (program-exit-status (hash-ref program-table pid)))

;; (program-asan-message pid)
;; Returns the asan error message of the program.
;;
;; Arguments:
;;  pid - PID of program.
;; Returns:
;;  #f if program is not done, exit status otherwise
(: program-asan-message (-> Integer (U False Bytes)))
(define (program-asan-message pid)
  (program-asan (hash-ref program-table pid)))

;; (program-destroy-handle pid) -> void?
;;
;; Arguments:
;;  pid - PID of program.
;;
;; Closes the internal-facing half of the I/O ports
;; for the process.
;; Also posts the process destroyed semaphore,
;; which will free up the PID entry in the table.
(: program-destroy-handle (-> Integer Void))
(define (program-destroy-handle pid)
  (call-with-semaphore
   program-destroy-semaphore
   (lambda ()
     (define pgrm (hash-ref program-table pid #f))
     (when (not (equal? pgrm #f))
       (match-define (program in-stdin in-stdout in-stderr
                              out-stdin out-stdout out-stderr
                              raw-stdin raw-stdout raw-stderr
                              handle control exit-status
                              destroyed-semaphore mode custodian
                              source-dir asan pty)
                     pgrm)

       ;; Note: ports are Racket pipes and therefore GC'd.
       ;; We don't need to close them.  (Closing the input port
       ;; cause a bit of a race condition with the dispatch code.
       ;; We don't close out-stdout and out-stderr as clients
       ;; may still be using them.  Note that in-stdout and in-stderr
       ;; MUST be closed for EOF to be properly sent).
       (hash-remove! program-table pid)

       ;; Post the destroyed semaphore
       (semaphore-post destroyed-semaphore)))))

;; Retrieve asan error message at "/tmp/seashell-asan.{pid}"
;;  this is set in "run-program"
(: delete-read-asan (-> Integer Bytes))
(define (delete-read-asan pid)
  (define path (string->path (format "/tmp/seashell-asan-~a.~a" (ormap (lambda (x) x) (list (getenv "USER") (getenv "LOGNAME") "")) pid)))
  (cond [(file-exists? path)
         (define contents (file->bytes path))
         (delete-file path)
         contents]
        [else #""]))

