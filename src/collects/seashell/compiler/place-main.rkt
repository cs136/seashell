#lang racket
;; Seashell's Clang interface.
;; Copyright (C) 2013-2014 The Seashell Maintainers.
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
(require seashell/compiler/compiler)
(require seashell/log)
(require seashell/seashell-config)
(provide seashell-compiler-place)

;; (seashell-compiler-place channel)
;; Invokes seashell-compile-files in a separate place,
;; preserving parallelism in Racket.
;;
;; This is the main function for the place.
(define (seashell-compiler-place channel)
  ;; These two things should not fail.
  (config-refresh!)
  (standard-logger-setup)
  (let loop ()
    (with-handlers
      ([exn:fail?
        (lambda (exn) (place-channel-put channel (list #t #f (exn-message exn))))])
      (define args (place-channel-get channel))
      (define-values (result data) (apply seashell-compile-files args))
      (place-channel-put channel (list #f result data)))
    (loop)))
