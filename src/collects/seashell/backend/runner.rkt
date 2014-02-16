#lang racket
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
         seashell/seashell-config)

(provide run-program program-stdin program-stdout program-stderr
         program-wait-evt program-kill program-status program-destroy-handle)

;; Global definitions and concurrency control primitives.
(struct program (in-stdin in-stdout in-stderr
                          out-stdin out-stdout out-stderr
                          raw-stdin raw-stdout raw-stderr
                          handle control exit-status
                          destroyed-semaphore) #:transparent #:mutable)
(struct exn:program:run exn:fail:user ())

(define program-table (make-hash))
(define program-new-semaphore (make-semaphore 1))
(define program-destroy-semaphore (make-semaphore 1))

;; (program-control-thread program)
;; Control thread helper for a running program.
;;
;; Arguments:
;;  program - Program structure.
;; Returns:
;;  Nothing.
(define (program-control-thread pgrm)
  (-> program? void?)
  (match-define (program in-stdin in-stdout in-stderr
                         out-stdin out-stdout out-stderr
                         raw-stdin raw-stdout raw-stderr
                         handle control exit-status
                         destroyed-semaphore)
    pgrm)
  (define pid (subprocess-pid handle))
  (define (close)
    (close-input-port raw-stdout)
    (close-input-port raw-stderr)
    (close-input-port in-stdin)
    (close-output-port raw-stdin)
    ;; These two ports must be closed, despite being pipes.
    ;; We depend on EOF being sent.
    (close-output-port out-stderr)
    (close-output-port out-stdout)
    (void))
  
  (let loop ()
    (match (sync/timeout 30 handle (thread-receive-evt) in-stdin raw-stderr raw-stdout)
      [(? (lambda (evt) (eq? handle evt))) ;; Program quit
       (logf 'info "Program with PID ~a quit with status ~a." pid (subprocess-status handle))
       (set-program-exit-status! pgrm (subprocess-status handle))
       (close)]
      [#f ;; Program timed out (30 seconds pass without any event)
       (logf 'info "Program with PID ~a timed out." pid)
       (set-program-exit-status! pgrm 255)
       (subprocess-kill handle #t)
       (close)]
      [(? (lambda (evt) (eq? thread-receive-evt evt))) ;; Received a signal.
       (match (thread-receive)
         ['kill
          (logf 'info "Program with PID ~a killed." pid (subprocess-status handle))
          (set-program-exit-status! program 254)
          (subprocess-kill handle #t)
          (close)])]
      [(? (lambda (evt) (eq? in-stdin evt))) ;; Received input from user
       (define input (make-bytes 4096))
       (define read (read-bytes-avail! input in-stdin))
       (when (integer? read)
         (write-bytes input raw-stdin 0 read))
       (loop)]
      [(? (lambda (evt) (eq? raw-stdout evt))) ;; Received output from program
       (define output (make-bytes 4096))
       (define read (read-bytes-avail! output raw-stdout))
       (when (integer? read)
         (write-bytes output out-stdout 0 read))
       (loop)]
      [(? (lambda (evt) (eq? raw-stderr evt))) ;; Received standard error from program
       (define output (make-bytes 4096))
       (define read (read-bytes-avail! output raw-stderr))
       (when (integer? read)
         (write-bytes output out-stderr 0 read))
       (loop)])))

;; (run-project program)
;;  Runs a program.
;;
;; Returns:
;;  PID of program
(define/contract (run-program binary)
  (-> path-string? integer?)
  (call-with-semaphore
    program-new-semaphore
    (thunk
      (with-handlers
          [(exn:fail:filesystem?
            (lambda (exn)
              (raise (exn:program:run
                      (format "Could not run binary: Received filesystem error: ~a" (exn-message exn))
                      (current-continuation-marks)))))]
        ;; Set the environment variables
        (putenv "ASAN_SYMBOLIZER_PATH" (path->string (read-config 'llvm-symbolizer)))
        ;; Find the binary
        (logf 'info "Running binary ~a" binary)
        ;; Run it.
        (define-values (handle raw-stdout raw-stdin raw-stderr)
          (subprocess #f #f #f binary))
        ;; Construct the I/O ports.
        (define-values (in-stdout out-stdout) (make-pipe))
        (define-values (in-stdin out-stdin) (make-pipe))
        (define-values (in-stderr out-stderr) (make-pipe))
        ;; Set buffering modes
        (file-stream-buffer-mode raw-stdout 'none)
        (file-stream-buffer-mode raw-stdin 'none)
        (file-stream-buffer-mode raw-stderr 'none)
        ;; Construct the destroyed-semaphore
        (define destroyed-semaphore (make-semaphore 0))
        ;; Construct the control structure.
        (define result (program in-stdin in-stdout in-stderr
                                out-stdin out-stdout out-stderr
                                raw-stdin raw-stdout raw-stderr
                                handle #f #f destroyed-semaphore))
        (define control-thread (thread (thunk (program-control-thread result))))
        (set-program-control! result control-thread)
        ;; Install it in the hash-table
        (define pid (subprocess-pid handle))
        ;; Block if there's still a PID in there.  This is to prevent
        ;; nasty race-conditions in which a process which is started
        ;; has the same PID as a process which is just about to be destroyed.
        (define block-semaphore
          (call-with-semaphore
            program-destroy-semaphore
            (thunk
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
               'kill)
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
    (thunk 
      (define pgrm (hash-ref program-table pid))
      (match-define (program in-stdin in-stdout in-stderr
                             out-stdin out-stdout out-stderr
                             raw-stdin raw-stdout raw-stderr
                             handle control exit-status
                             destroyed-semaphore)
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
