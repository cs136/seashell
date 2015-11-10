#lang racket/base
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
         seashell/seashell-config
         seashell/diff
         racket/serialize
         racket/contract
         racket/match
         racket/port
         racket/path
         racket/file)

(provide run-program program-stdin program-stdout program-stderr
         program-wait-evt program-kill program-status program-destroy-handle
         program-mode)

;; Global definitions and concurrency control primitives.
(struct program (in-stdin in-stdout in-stderr
                          out-stdin out-stdout out-stderr
                          raw-stdin raw-stdout raw-stderr
                          handle control exit-status
                          destroyed-semaphore
                          _mode) #:transparent #:mutable)
(struct exn:program:run exn:fail:user ())

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
(define/contract (program-control-test-thread pgrm test-name input expected)
  (-> program? string? bytes? (or/c #f bytes?) void?)
  (match-define (program in-stdin in-stdout in-stderr
                         out-stdin out-stdout out-stderr
                         raw-stdin raw-stdout raw-stderr
                         handle control exit-status
                         destroyed-semaphore mode)
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
        (lambda (exn)
          (logf 'info "write-bytes failed to write ~a.in to program stdin: received error: ~a"
                      test-name (exn-message exn))))]
      (write-bytes input raw-stdin))
    (close-output-port raw-stdin)))

  ;; Background read stuff.
  (define-values (buf-stderr cp-stderr) (make-pipe))
  (define-values (buf-stdout cp-stdout) (make-pipe))
  (define stderr-thread (thread (lambda ()
            (logf 'debug "Starting copy port for stderr for program PID ~a." pid)
            (copy-port raw-stderr cp-stderr))))
  (define stdout-thread (thread (lambda ()
            (logf 'debug "Starting copy port for stdout for program PID ~a." pid)
            (copy-port raw-stdout cp-stdout))))

  ;; Helper to close ports
  (define (close)
    (close-input-port raw-stdout)
    (close-input-port raw-stderr)
    (close-output-port out-stdout))

  (define receive-evt (thread-receive-evt))
  (let loop ()
    (match (sync/timeout 30
                         handle
                         receive-evt)
      [(? (lambda (evt) (eq? handle evt))) ;; Program quit
       (logf 'info "Program with PID ~a quit with status ~a." pid (subprocess-status handle))
       (set-program-exit-status! pgrm (subprocess-status handle))
       ;; Wait for threads to finish copying
       (thread-wait stderr-thread)
       (thread-wait stdout-thread)
       ;; Read stdout, stderr.
       (close-output-port cp-stderr)
       (close-output-port cp-stdout)
       (define stdout (port->bytes buf-stdout))
       (define stderr (port->bytes buf-stderr))

       (match (subprocess-status handle)
         [0
          ;; Three cases:
          (cond
            [(not expected)
             (write (serialize `(,pid ,test-name "no-expect" ,stdout ,stderr)) out-stdout)]
            [(equal? stdout expected)
             (write (serialize `(,pid ,test-name "passed")) out-stdout)]
            [else
              ;; Split expected and output, difference them.
              (define output-lines (regexp-split #rx"\n" stdout))
              (define expected-lines (regexp-split #rx"\n" expected))
              ;; hotfix: diff with empty because it's slow
              (write (serialize `(,pid ,test-name "failed" ,(list-diff expected-lines '()) ,stderr ,stdout)) out-stdout)])]
         [_ (write (serialize `(,pid ,test-name "error" ,(subprocess-status handle) ,stderr)) out-stdout)])
       (logf 'debug "Done sending test results for program PID ~a." pid)
       (close)]
      [#f ;; Program timed out (30 seconds pass without any event)
       (logf 'info "Program with PID ~a timed out." pid)
       (set-program-exit-status! pgrm 255)
       (subprocess-kill handle #t)
       (write (serialize `(,pid ,test-name "timeout")) out-stdout)
       (close)]
      [(? (lambda (evt) (eq? receive-evt evt))) ;; Received a signal.
       (match (thread-receive)
         ['kill
          (logf 'info "Program with PID ~a killed." pid)
          (set-program-exit-status! pgrm 254)
          (subprocess-kill handle #t)
          (write (serialize `(,pid ,test-name "killed")) out-stdout)
          (close)])]))
  (void))

;; (program-control-thread program)
;; Control thread helper for a running program.
;;
;; Arguments:
;;  program - Program structure.
;; Returns:
;;  Nothing.
(define/contract (program-control-thread pgrm)
  (-> program? void?)
  (match-define (program in-stdin in-stdout in-stderr
                         out-stdin out-stdout out-stderr
                         raw-stdin raw-stdout raw-stderr
                         handle control exit-status
                         destroyed-semaphore mode)
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
    (void))

  ;; Deal with receiving signals,
  ;; in all cases. (so I/O does not block a kill signal)
  (define (check-signals tail)
    (if (sync/timeout 0 (thread-receive-evt))
       (match (thread-receive)
         ['kill
          (logf 'info "Program with PID ~a killed." pid)
          (set-program-exit-status! pgrm 254)
          (subprocess-kill handle #t)
          (close)])
       (tail)))

  (let loop ()
    (define receive-evt (thread-receive-evt))
    (match (sync/timeout 30
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
       (set-program-exit-status! pgrm (subprocess-status handle))
       ;; Flush the ports!
       (define read-stdout
         (if (port-closed? raw-stdout)
           eof
           (port->bytes raw-stdout)))
       (unless (eof-object? read-stdout)
         (write-bytes read-stdout out-stdout))
       (define read-stderr
         (if (port-closed? raw-stderr)
           eof
           (port->bytes raw-stderr)))
       (unless (eof-object? read-stderr)
         (write-bytes read-stderr out-stderr))
       (close)]
      [#f ;; Program timed out (30 seconds pass without any event)
       (logf 'info "Program with PID ~a timed out." pid)
       (set-program-exit-status! pgrm 255)
       (subprocess-kill handle #t)
       (close)]
      [(? (lambda (evt) (eq? receive-evt evt))) ;; Received a signal.
       (check-signals loop)]
      [(? (lambda (evt) (eq? in-stdin evt))) ;; Received input from user
       (define input (make-bytes (read-config 'io-buffer-size)))
       (define read (read-bytes-avail! input in-stdin))
       (when (integer? read)
         (write-bytes input raw-stdin 0 read))
       (when (eof-object? read)
         (close-input-port in-stdin)
         (close-output-port raw-stdin))
       (check-signals loop)]
      [(? (lambda (evt) (eq? raw-stdout evt))) ;; Received output from program
       (define output (make-bytes (read-config 'io-buffer-size)))
       (define read (read-bytes-avail! output raw-stdout))
       (when (integer? read)
         (write-bytes output out-stdout 0 read))
       (when (eof-object? read)
         (close-input-port raw-stdout)
         (close-output-port out-stdout))
       (check-signals loop)]
      [(? (lambda (evt) (eq? raw-stderr evt))) ;; Received standard error from program
       (define output (make-bytes (read-config 'io-buffer-size)))
       (define read (read-bytes-avail! output raw-stderr))
       (when (integer? read)
         (write-bytes output out-stderr 0 read))
       (when (eof-object? read)
         (close-input-port raw-stderr)
         (close-output-port out-stderr))
       (check-signals loop)]))
  (void))

;; (run-program binary directory lang test)
;;  Runs a program.
;;
;; Arguments:
;;   program   - the filename of the program to be run
;;   directory - the directory containing the program
;;   lang      - the language with which to run the program ('C or 'racket)
;;   test      - if #f, I/O is done directly with the user. Otherwise, test is
;;               a string representing the name of a test to use in tests/.
;;               i.e. if test is "foo", input is from tests/foo.in, output is
;;               diffed against tests/foo.expect
;;
;; Returns:
;;  PID - handle representing the run.  On a test, test results are written
;;    to stdout.  On an interactive run, data is forwarded to/from
;;    the program.
(define/contract (run-program binary directory lang test)
  (-> path-string? path-string? (or/c 'C 'racket) (or/c #f string?) integer?)
  (call-with-semaphore
    program-new-semaphore
    (lambda ()
      (with-handlers
          [(exn:fail:filesystem?
            (lambda (exn)
              (raise (exn:program:run
                      (format "Could not run binary: Received filesystem error: ~a" (exn-message exn))
                      (current-continuation-marks)))))]
        (if test
          (logf 'info "Running file ~a with language ~a using test ~a" binary lang test)
          (logf 'info "Running file ~a with language ~a" binary lang))

        (define-values (handle raw-stdout raw-stdin raw-stderr)
          (parameterize
            ([current-directory directory]
             [current-environment-variables (environment-variables-copy (current-environment-variables))])
            (match lang
              ['C
                ;; Behaviour is inconsistent if we just exec directly.
                ;; This seems to work.  (why: who knows?)
                (putenv "ASAN_OPTIONS" "detect_leaks=1:stack_trace_format=\"{'frame': %n, 'module': '%m', 'offset': '%o', 'function': '%f', 'function_offset': '%q', 'file': '%s', 'line': %l, 'column': %c}\"")
                (putenv "ASAN_SYMBOLIZER_PATH" (some-system-path->string (read-config 'llvm-symbolizer)))
                (subprocess #f #f #f binary)]
              ['racket (subprocess #f #f #f (read-config 'racket-interpreter)
                                   "-t" 
                                    (some-system-path->string (read-config 'seashell-racket-runtime-library))
                                   "-u" binary)])))

            ;; Construct the I/O ports.
            (define (make-io-pipe)
              (if test (make-pipe) (make-pipe (read-config 'io-buffer-size))))

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
                                    (if test 'test 'run)))
            (define control-thread
              (thread
                (lambda ()
                  (if test
                    (program-control-test-thread result
                                                 test
                                                 (file->bytes (build-path directory (read-config 'tests-subdirectory) (string-append test ".in")))
                                                 (with-handlers
                                                   ([exn:fail:filesystem? (lambda (exn) #f)])
                                                   (file->bytes (build-path directory (read-config 'tests-subdirectory) (string-append test ".expect")))))
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
                  (if handle (program-destroy-semaphore handle) #f))))
            (when block-semaphore (sync (semaphore-peek-evt block-semaphore)))
            (hash-set! program-table pid result)
            (logf 'info "Binary ~a running as PID ~a." binary pid)
            pid))))

;; (program-stdin pid)
;; Returns the standard input port for a program
;;
;; Arguments:
;;  pid - PID of program.
;; Returns:
;;  Port to write to program's standard input.
(define/contract (program-stdin pid)
  (-> integer? output-port?)
  (program-out-stdin (hash-ref program-table pid)))

;; (program-mode pid)
;; Returns if the program is running in regular or test mode.
;; 
;; Arguments:
;;  pid - PID of program.
;; Returns:
;;  'test if running in test mode.
;;  'run if in regular mode.
(define/contract (program-mode pid)
  (-> integer? symbol?)
  (program-_mode (hash-ref program-table pid)))

;; (program-stdout pid)
;; Returns the standard output port for a program.
;;
;; Arguments:
;;  pid - PID of program.
;; Returns:
;;  Port to read from program's standard output.
(define/contract (program-stdout pid)
  (-> integer? input-port?)
  (program-in-stdout (hash-ref program-table pid)))

;; (program-stderr pid)
;; Returns the standard error port for a program.
;;
;; Arguments:
;;  pid - PID of program.
;; Returns:
;;  Port to read from program's standard error.
(define/contract (program-stderr pid)
  (-> integer? input-port?)
  (program-in-stderr (hash-ref program-table pid)))

;; (program-kill pid)
;; Kills a program
;;
;; Aguments:
;;  pid - PID of program.
;; Returns:
;;  Nothing
(define/contract (program-kill pid)
  (-> integer? void?)
  (thread-send (program-control (hash-ref program-table pid))
               'kill
               #f)
  (void))

;; (program-wait-evt pid)
;; Gets the event that will be ready when the program is done.
;;
;; Arguments:
;;  pid - PID of program.
;; Returns:
;;  An event that is ready when the program is done.
(define/contract (program-wait-evt pid)
  (-> integer? evt?)
  (program-control (hash-ref program-table pid)))

;; (program-status pid)
;; Returns the exit status of the program.
;;
;; Arguments:
;;  pid - PID of program.
;; Returns:
;;  #f if program is not done, exit status otherwise
(define/contract (program-status pid)
  (-> integer? (or/c #f integer?))
  (program-exit-status (hash-ref program-table pid)))

;; (program-destroy-handle pid) -> void?
;;
;; Arguments:
;;  pid - PID of program.
;;
;; Closes the internal-facing half of the I/O ports
;; for the process.
;; Also posts the process destroyed semaphore,
;; which will free up the PID entry in the table.
(define/contract (program-destroy-handle pid)
  (-> integer? void?)
  (call-with-semaphore
    program-destroy-semaphore
    (lambda ()
      (define pgrm (hash-ref program-table pid))
      (match-define (program in-stdin in-stdout in-stderr
                             out-stdin out-stdout out-stderr
                             raw-stdin raw-stdout raw-stderr
                             handle control exit-status
                             destroyed-semaphore mode)
        pgrm)

      ;; Note: ports are Racket pipes and therefore GC'd.
      ;; We don't need to close them.  (Closing the input port
      ;; cause a bit of a race condition with the dispatch code.
      ;; We don't close out-stdout and out-stderr as clients
      ;; may still be using them.  Note that in-stdout and in-stderr
      ;; MUST be closed for EOF to be properly sent).
      (hash-remove! program-table pid)

      ;; Post the destroyed semaphore
      (semaphore-post destroyed-semaphore))))
