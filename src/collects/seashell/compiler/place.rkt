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
(require seashell/compiler/compiler)
(provide seashell-compile-files/place)

(require/typed racket/serialize
               [deserialize (-> Any Any)])
(: compiler-place (U False Place))
(define compiler-place #f)

;; seashell-compile-files/place
;; Invokes seashell-compile-files in a separate place,
;; preserving parallelism in Racket.
(: seashell-compile-files/place (-> (Listof String) (Listof String) (Listof Path) (Listof Path)
                                    (Values (U Bytes False) Seashell-Diagnostic-Table)))
(define (seashell-compile-files/place user-cflags user-ldflags sources objects)
  (unless compiler-place
    (seashell-compile-place/init))
  (cond
    [(and (place? compiler-place)
          (not (sync/timeout 0 (place-dead-evt (cast compiler-place Place)))))
      (define-values (read-end write-end) (place-channel))
      (place-channel-put (cast compiler-place Place)
                         (list write-end user-cflags user-ldflags sources objects))
      (match-define
        (list exn? result data)
        (deserialize (place-channel-get read-end)))
      (when exn?
            (raise (exn:fail (format "Exception raised in compiler place: ~a!" data)
                             (current-continuation-marks))))
      (values (cast result (U False Bytes))
              (cast data Seashell-Diagnostic-Table))]
    [else (values #f (make-hash))]))

;; seashell-compile-place/init
;; Sets up the place for compilation.
(define (seashell-compile-place/init)
  ;; Make sure that we don't use current-output-port
  ;; or the detach will fail.
  (define-values (place in out err)
    (dynamic-place* 'seashell/compiler/place-main
                    'seashell-compiler-place
                    #:out (current-error-port)
                    #:err (current-error-port)))
  (assert (port? in))
  (set! compiler-place place)
  (close-output-port in))
