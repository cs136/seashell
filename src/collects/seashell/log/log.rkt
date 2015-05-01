#lang racket
;; Seashell collection
;; Copyright (C) 2013-2015 The Seashell Maintainers.
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
(require seashell/seashell-config)
(provide logf make-port-logger standard-logger-setup format-stack-trace)

(define logger (make-logger 'seashell))
(define log-ts-str "[~a-~a-~a ~a:~a:~a ~a]")

;; log-ts-args
;; Generates a list of values to substitute into the logging string.
(define (log-ts-args)
  (define (pad-left z i)
    (string-append (make-string (- z (string-length (number->string i))) #\0)
                    (number->string i)))
  (let ((dt (seconds->date (current-seconds))))
    (list
      (pad-left 4 (date-year dt))
      (pad-left 2 (date-month dt))
      (pad-left 2 (date-day dt))
      (pad-left 2 (date-hour dt))
      (pad-left 2 (date-minute dt))
      (pad-left 2 (date-second dt))
      (string-append (number->string (quotient (date-time-zone-offset dt) 3600))
                    (pad-left 2 (remainder (date-time-zone-offset dt) 3600))))))

;; logf: category format args... -> void?
;; Writes an entry into the logger.
(define/contract (logf category format-string . args)
  (->* ((or/c 'fatal 'error 'warning 'info 'debug) string?)
       #:rest (listof any/c) 
       void?)
  (unless (read-config 'test-mode)
    (define block (make-semaphore))
    (log-message logger
                 category
                 #f
                 (apply format `(,(string-append log-ts-str " ~a: " format-string)
                    ,@(log-ts-args)
                    ,category
                    ,@args))
                 (list (current-continuation-marks) block))
    ;; Don't block if debug and not debug mode.
    (unless (and (eq? category 'debug) (not (read-config 'debug)))
      (semaphore-wait block)))
  (void))

;; make-log-reader: type-regexp -> evt?
;; Creates a log receiver that receives all messages at level or higher.
;;
;; Arguments:
;;  level - 'fatal, 'error, 'warning, 'info, 'debug
;; Returns:
;;  A log receiver.
(define/contract (make-log-reader level)
  (-> (or/c 'fatal 'error 'warning 'info 'debug) log-receiver?)
  (make-log-receiver logger level))

;; format-stack-trace
;; Formats a stack trace (continuation-mark-set).
;;
;; Arguments:
;;  trace - Continuation mark set.
;; Returns:
;;  String representing a prettified stack trace.
(define/contract (format-stack-trace trace)
  (-> continuation-mark-set? string?)
  (string-join
    `(,@(for/list ([item (in-list (continuation-mark-set->context trace))])
         (format "~a at:\n  ~a\n"
                  (if (car item)
                      (car item)
                      "<unknown procedure>")
                  (if (cdr item)
                      (format "line ~a, column ~a, in file ~a"
                              (srcloc-line (cdr item))
                              (srcloc-column (cdr item))
                              (srcloc-source (cdr item)))
                      "<unknown location>"))))
    ""))

;; make-port-logger
;; Creates a thread that receives events at level or higher
;; and writes it to the specified port.
(define/contract (make-port-logger level port)
  (-> (or/c 'fatal 'error 'warning 'info 'debug) output-port? thread?)
  (define reader (make-log-reader level))
  (thread
    (thunk
      (let loop ()
        (match (sync reader)
          [(vector level message (list marks block) _)
           (cond
             [(and (read-config 'debug) (or (equal? level 'fatal) (equal? level 'error)))
              (fprintf port "~a~n***Stacktrace follows***~n~a~n***End Stacktrace***~n" 
                       message
                       (format-stack-trace marks))]
             [else
              (fprintf port "~a~n" message)])
           (flush-output port)
           (semaphore-post block)]
          [anything
           (fprintf port "warning: unknown message ~a~n" anything)])
        (loop)))))

;; standard-logger-setup
;; Sets up the logger in a standard fashion - writing to standard error
;; all messages at >debug in regular mode, all messages at >=debug when
;; debugging.
(define (standard-logger-setup)
  (if (read-config 'debug)
    (make-port-logger 'debug (current-error-port))
    (make-port-logger 'info (current-error-port))))
