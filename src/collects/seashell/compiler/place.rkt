#lang racket/base
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
         racket/contract
         racket/match
         racket/place
         racket/bool
         seashell/compiler/ffi)
(provide seashell-compile-files/place
         seashell-compile-place/init)

(define compiler-place #f)
(define compiler-place-lock (make-semaphore 1))

;; seashell-compile-files/place 
;; Invokes seashell-compile-files in a separate place,
;; preserving parallelism in Racket.
(define/contract (seashell-compile-files/place user-cflags user-ldflags sources objects)
  (-> (listof string?) (listof string?) (listof path?) (listof path?)
      (values (or/c bytes? false?) (hash/c path? (listof seashell-diagnostic?))))
  (cond
    [(and compiler-place (not (sync/timeout 0 (place-dead-evt compiler-place))))
      (define result-channel (make-channel))
      ;; This runs with the main custodian, as an early, unexpected shutdown
      ;; of this thread will cause the compiler to end up in an inconsistent state.
      (compiler_kernel_thread
        (lambda ()
          (call-with-semaphore
            compiler-place-lock
            (lambda ()
              (place-channel-put compiler-place 
                                 (list user-cflags user-ldflags sources objects))
              (match-define 
                (list exn? result data)
                (place-channel-get compiler-place))
              (cond
                [exn?
                 (channel-put result-channel
                              (exn:fail (format "Exception raised in compiler place: ~a!" data)
                                        (current-continuation-marks)))]
                [else
                 (channel-put result-channel
                              (list result data))])))))
      (match (channel-get result-channel)
        [(? exn? exn) (raise exn)]
        [(list result data) (values result data)])]
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
  (set! compiler-place place)
  (close-output-port in))
