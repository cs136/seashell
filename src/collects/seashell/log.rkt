#lang typed/racket
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
(require (submod seashell/seashell-config typed)
         "utils/sentry.rkt"
         "support-native.rkt"
         typed/json)
(provide logf make-port-logger standard-logger-setup format-stack-trace
         tracef capture-exception)

(define logger (make-logger 'seashell-all))
(define trace-logger (make-logger 'seashell-api-trace logger))
(define message-logger (make-logger 'seashell logger))
(define reporter (new sentry-reporter% [opt-dsn #f])) ;; -- default uninitialized, we set it up properly
                                                      ;; in standard-logger-setup as the configuration
                                                      ;; hasn't been loaded yet.

(define log-ts-str "[~a-~a-~a ~a:~a:~a ~a]")

;; log-ts-args
;; Generates a list of values to substitute into the logging string.
(: log-ts-args (-> (Listof String)))
(define (log-ts-args)
  (: pad-left (-> Nonnegative-Fixnum Number String))
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

;; logf/tracef: category format args... -> void?
;; Writes an entry into the message/tracer logger.
(: logf-to (-> Logger Log-Level String (Listof Any) Void))
(define (logf-to logger category format-string args)
  (unless (read-config-boolean 'test-mode)
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
    (unless (and (eq? category 'debug) (not (read-config-boolean 'debug)))
      (semaphore-wait block)))
  (void))
(: logf (-> Log-Level String Any * Void))
(define (logf category format-string . args)
  (logf-to message-logger category format-string args))
(: tracef (-> Log-Level String Any * Void))
(define (tracef category format-string . args)
  (logf-to trace-logger category format-string args))

;; traced-raise exn -> Any
;; Raises an exception, logging to Sentry and to the trace logger.
(: traced-raise (-> exn Any))
(define (traced-raise exn)
  (tracef 'error "An exception was raised: ~a" (exn-message exn))
  (capture-exception exn)
  (raise exn))

;; capture-exception exn -> Any
;; Captures an exception, sending it to Sentry
(: capture-exception (-> exn Any))
(define (capture-exception exception)
  (sync/timeout (read-config-nonnegative-real 'sentry-delay)
    (thread
      (thunk
        (with-handlers ([exn? (lambda ([e : exn]) (logf 'warning "Error sending exception: ~a." (exn-message e)))])
          ;; We block on the reporter here
          (send reporter report-exception exception #{`#hasheq() :: (HashTable Symbol JSExpr)} #t))))))

;; make-log-reader: level -> log-receiver
;; Creates a log receiver that receives all messages at level or higher.
;;
;; Arguments:
;;  level - 'fatal, 'error, 'warning, 'info, 'debug
;; Returns:
;;  A log receiver.
(: make-log-reader (-> Log-Level Log-Receiver))
(define (make-log-reader level)
  (make-log-receiver logger level))

;; format-stack-trace
;; Formats a stack trace (continuation-mark-set).
;;
;; Arguments:
;;  trace - Continuation mark set.
;; Returns:
;;  String representing a prettified stack trace.
(: format-stack-trace (-> Continuation-Mark-Set String))
(define (format-stack-trace trace)
  (apply string-append
    `(,@(for/list : (Listof String)
                  ([item : (Pairof (U False Symbol) Any) (in-list (continuation-mark-set->context trace))])
         (format "~a at:\n  ~a\n"
                  (if (car item)
                      (car item)
                      "<unknown procedure>")
                  (if (cdr item)
                      (begin
                        (assert (srcloc? (cdr item)))
                        (format "line ~a, column ~a, in file ~a"
                                (srcloc-line (cdr item))
                                (srcloc-column (cdr item))
                                (srcloc-source (cdr item))))
                      "<unknown location>"))))))

;; make-port-logger
;; Creates a thread that receives events at level or higher
;; and writes it to the specified port.
(: make-port-logger (-> Log-Level Output-Port Thread))
(define (make-port-logger level port)
  (define reader (make-log-reader level))
  (thread
    (lambda ()
      (let loop ()
        (match (sync reader)
          [(vector level message (list marks block) _)
           (assert (semaphore? block))
           (assert (continuation-mark-set? marks))
           (cond
             [(and (read-config-boolean 'debug) (or (equal? level 'fatal) (equal? level 'error)))
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

;; make-sentry-logger
;; Creates a thread that receives events from the log (not trace) logger and forwards them to Sentry
(: make-sentry-logger (-> Log-Level Thread))
(define (make-sentry-logger level)
  (define reader (make-log-receiver message-logger level))
  (thread
    (lambda ()
      (let loop ()
        (match (sync reader)
          [(vector level message (list marks block) _)
           (send reporter capture-log-entry level message (current-seconds))]
          [_ #f])
        (loop)))))

;; standard-logger-setup
;; Sets up the logger in a standard fashion - writing to standard error
;; all messages at >debug in regular mode, all messages at >=debug when
;; debugging.
(: standard-logger-setup (-> Void))
(define (standard-logger-setup)
  (define user (seashell_get_username))
  (define userid (format "~a@~a" user (read-config-string 'domain)))
  (set! reporter (new sentry-reporter% [opt-dsn (read-config-optional-string 'sentry-target)]
                                       [origin (read-config-optional-string 'sentry-origin)]))
  (send reporter set-user! user)
  (send reporter set-email! userid)
  (if (read-config-boolean 'debug)
    (begin (make-port-logger 'debug (current-error-port)) (make-sentry-logger 'debug))
    (begin (make-port-logger 'debug (current-error-port)) (make-sentry-logger 'debug)))
  (void))
