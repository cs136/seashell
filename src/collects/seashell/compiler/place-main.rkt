#lang typed/racket
;; Seashell's Clang interface.
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
(require seashell/compiler/compiler
         seashell/log
         (submod seashell/seashell-config typed))
(provide seashell-compiler-place)

;; (seashell-compiler-place/thread write-end . args)
;; Thread that actually processes the compilation request.
(: seashell-compiler-place/thread (-> Place-Channel (Listof String) (Listof String) (Listof Path) (Listof Path) Thread))
(define (seashell-compiler-place/thread write-end cflags ldflags cfiles ofiles)
  (thread
    (lambda ()
      (with-handlers
        ([exn:fail?
          (lambda ([exn : exn]) (place-channel-put write-end (list #t #f (exn-message exn)))
            (logf 'error "Exception raised in compilation place ~a: ~a" (exn-message exn) (format-stack-trace (exn-continuation-marks exn))))])
        (define-values (result data) (seashell-compile-files cflags ldflags cfiles ofiles))
        (place-channel-put write-end (list #f result (seashell-diagnostic-table->bytes data)))))))

;; (seashell-compiler-place channel)
;; Invokes seashell-compile-files in a separate place,
;; preserving parallelism in Racket.
;;
;; This is the main function for the place.
(: seashell-compiler-place (-> Place-Channel Any))
(define (seashell-compiler-place channel)
  ;; These two things should not fail.
  (config-refresh!)
  (standard-logger-setup)
  (let loop : Any ()
    (with-handlers
      ([exn:fail?
        (lambda ([exn : exn]) (place-channel-put channel (list #t #f (exn-message exn)))
          (logf 'error "Exception raised in compilation place ~a: ~a" (exn-message exn) (format-stack-trace (exn-continuation-marks exn))))])
      (match-define (list chan cflags ldflags cfiles ofiles)
        (place-channel-get channel))
      (seashell-compiler-place/thread (cast chan Place-Channel)
                                      (cast cflags (Listof String))
                                      (cast ldflags (Listof String))
                                      (cast cfiles (Listof Path))
                                      (cast ofiles (Listof Path))))
    (loop)))
