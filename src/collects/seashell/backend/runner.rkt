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
         seashell/backend/project
         seashell/seashell-config)

(provide run-project program-stdin program-stdout program-stderr
         program-runtime-stderr
         program-wait-evt program-kill program-status)

;; Global definitions and concurrency control primitives.
(struct program (in-stdin in-stdout in-program-stderr in-runtime-stderr
                          out-stdin out-stdout out-program-stderr out-runtime-stderr
                          raw-stdin raw-stdout raw-stderr
                          handle control exit-status) #:transparent #:mutable)
(struct exn:project:run exn:project ())
(define program-table (make-hash))

;; (program-control-thread program)
;; Control thread helper for a running program.
;;
;; Arguments:
;;  program - Program structure.
;; Returns:
;;  Nothing.
(define (program-control-thread pgrm)
  (-> program? void?)
  (match-define (program in-stdin in-stdout in-program-stderr in-runtime-stderr
                         out-stdin out-stdout out-program-stderr out-runtime-stderr
                         raw-stdin raw-stdout raw-stderr
                         handle control exit-status)
    pgrm)
  (define pid (subprocess-pid handle))
  (define (close)
    (close-input-port raw-stdout)
    (close-input-port raw-stderr)
    (close-input-port in-stdin)
    (close-output-port raw-stdin)
    (close-output-port out-program-stderr)
    (close-output-port out-runtime-stderr)
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
          (set-program-exit-status! program 254)
          (subprocess-kill handle #t)
          (close)])]
      [(? (lambda (evt) (eq? in-stdin evt))) ;; Received input from user
       (define input (make-bytes 256))
       (define read (read-bytes-avail! input in-stdin))
       (when (integer? read)
         (write-bytes input raw-stdin 0 read))
       (loop)]
      [(? (lambda (evt) (eq? raw-stdout evt))) ;; Received output from program
       (define output (make-bytes 256))
       (define read (read-bytes-avail! output raw-stdout))
       (when (integer? read)
         (write-bytes output out-stdout 0 read))
       (loop)]
      [(? (lambda (evt) (eq? raw-stderr evt))) ;; Received standard error from program
       ;; TODO: parse AddressSanitizer / Racket error messages and redirect them to runtime-stderr
       (define output (make-bytes 256))
       (define read (read-bytes-avail! output raw-stderr))
       (when (integer? read)
         (write-bytes output out-program-stderr 0 read))
       (loop)])))

;; (run-project project)
;;  Runs a project.
;;
;; Returns:
;;  PID of program
(define/contract (run-project project)
  (-> (and/c project-name? is-project?) integer?)
  ;; TODO: Racket mode.
  (with-handlers
      [(exn:fail:filesystem?
        (lambda (exn)
          (raise (exn:project:run
                  (format "Could not run binary: Received filesystem error: ~a" (exn-message exn))
                  (current-continuation-marks)))))]
    ;; Find the binary
    (define binary (check-and-build-path (read-config 'seashell) (string-append project "-binary")))
    (logf 'info "Running binary ~a for project ~a." binary project)
    ;; Run it.
    (define-values (handle raw-stdout raw-stdin raw-stderr)
      (subprocess #f #f #f binary))
    ;; Construct the I/O ports.
    (define-values (in-stdout out-stdout) (make-pipe))
    (define-values (in-stdin out-stdin) (make-pipe))
    (define-values (in-program-stderr out-program-stderr) (make-pipe))
    (define-values (in-runtime-stderr out-runtime-stderr) (make-pipe))
    ;; Construct the control structure.
    (define result (program in-stdin in-stdout in-program-stderr in-runtime-stderr
                            out-stdin out-stdout out-program-stderr out-runtime-stderr
                            raw-stdin raw-stdout raw-stderr
                            handle #f #f))
    (define control-thread (thread (thunk (program-control-thread result))))
    (set-program-control! result control-thread)
    ;; Install it in the hash-table
    (define pid (subprocess-pid handle))
    (hash-set! program-table pid result)
    (logf 'info "Binary ~a for project ~a running as PID ~a." binary project pid)
    pid))

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
  (program-in-program-stderr (hash-ref program-table pid)))

;; (program-runtime-stderr pid)
;; Returns the runtime standard error port for a program.
;;
;; Arguments:
;;  pid - PID of program.
;; Returns:
;;  Port to read from program's runtime standard error.
(define/contract (program-runtime-stderr pid)
  (-> integer? input-port?)
  (program-runtime-stderr (hash-ref program-table pid)))

;; (program-kill pid)
;; Kills a program
;;
;; Arguments:
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
