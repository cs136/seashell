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
(require seashell/log/multiplex
         racket/date
         racket/function
         racket/contract
         racket/port
         racket/async-channel
         racket/match)

(provide
  (contract-out
    [logf
      (->* (symbol? string?) #:rest (listof any/c) void?)]
    [logf/sync
      (->* (symbol? string?) #:rest (listof any/c) void?)]
    [make-log-reader
      (-> string? evt?)]
    [make-fs-logger
      (-> string? (or/c string? path?) thread?)]
    [make-port-logger
      (-> string? port? thread?)]))

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
      (list (symbol->string cat)
            (apply format `(,(string-append log-ts-str " ~a: " fmt)
                            ,@(log-ts-args)
                            ,cat
                            ,@args))))))

;; logf/sync: category format args... -> void
(define logf/sync
  (lambda(cat fmt . args)
    (define s (make-semaphore 0))
    (mt-send
      log-mtx
      (list s (symbol->string cat)
            (apply format `(,(string-append log-ts-str " ~a: " fmt)
                            ,@(log-ts-args)
                            ,cat
                            ,@args))))
    (semaphore-wait s)))

;; make-log-reader: type-regexp -> evt?
(define (make-log-reader type-regexp)
  (define filter-chan (make-async-channel))
  (define (courier chan fch)
    (async-channel-put fch 'ready)
    (let loop ()
      (match (mt-receive chan)
        [(list (regexp type-regexp) (? string? msg))
         (async-channel-put fch msg)]
        [(list (? semaphore? s) (regexp type-regexp) (? string? msg))
         (async-channel-put fch (cons s msg))]
        [else (void)])
      (loop)))
  (thread (thunk (courier (mt-subscribe log-mtx) filter-chan)))
  (async-channel-get filter-chan)
  filter-chan)

;; make-port-logger: type-regexp port -> thread
(define (make-port-logger type-regexp port)
  (define reader (make-log-reader type-regexp))
  (thread
    (thunk
      (let loop ()
        (match (sync reader)
          [(cons (? semaphore? s) (? string? msg))
           (fprintf port "~a~n" msg)
           (flush-output port)
           (semaphore-post s)]
          [(? string? msg)
           (fprintf port "~a~n" msg)
           (flush-output port)])
        (loop)))))

;; make-fs-logger: type-regexp file -> thread
(define (make-fs-logger type-regexp file)
    (call-with-output-file
      file
      ((curry make-port-logger) type-regexp) #:exists 'append))

