#lang racket/base
;; Seashell collection
;; Copyright (C) 2013 The Seashell Maintainers.
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
(require seashell/multiplex
         racket/date
         racket/function
         racket/contract
         racket/port
         racket/match)

(provide
  (contract-out
    [logf
      (->* (symbol? string?) #:rest (listof any/c) void?)]
    [make-log-reader
      (-> string? (-> string?))]
    [make-fs-logger
      (-> string? (or/c string? path?) thread?)]))

(define log-mtx (make-multiplexer))
(define log-ts-str "[~a-~a-~a ~a:~a:~a ~a]")

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

;; logf: category format args... -> void
(define logf
  (lambda(cat fmt . args)
    (mt-send
      log-mtx
      (cons cat
            (with-output-to-string
            (thunk
              (apply printf `(,(string-append log-ts-str " ~a: " fmt)
                              ,@(log-ts-args)
                              ,cat
                              ,@args))))))))

;; make-log-reader: type-regexp -> (func: -> message)
(define (make-log-reader type-regexp)
  (define chan (mt-subscribe log-mtx))
  (define (next-message)
    (match
        (let
            ((msg (mt-receive chan)))
          (cons (symbol->string (car msg)) (cdr msg)))
      [(cons (regexp type-regexp) (? string? msg)) msg]
      [else (next-message)]))
  next-message)

;; make-fs-logger: type-regexp file -> thread
(define (make-fs-logger type-regexp file)
  (define reader (make-log-reader type-regexp))
  (thread
    (thunk
    (call-with-output-file file
      (lambda(port)
        (let loop ()
          (fprintf port "~a~n" (reader))
          (flush-output port)
          (loop)))
      #:exists 'append))))
