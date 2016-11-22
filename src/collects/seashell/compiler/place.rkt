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
(require seashell/compiler/compiler seashell/log)
(provide seashell-compile-files/place seashell-compile-place/shutdown
         seashell-compile-place/alive?)

(module untyped racket/base
  (require ffi/unsafe ffi/unsafe/define)
  (define libmz (ffi-lib #f))
  (define-ffi-definer define-mzscheme libmz)
  (define-mzscheme scheme_make_custodian (_fun _pointer -> _scheme))
  (provide scheme_make_custodian))

(require/typed racket/serialize
               [deserialize (-> Any Any)])
(require/typed (submod "." untyped)
               [scheme_make_custodian (-> (U False Custodian) Custodian)])

(: global-compiler-place (Boxof (U False Place)))
(define global-compiler-place (box #f))
(: compile-place-semaphore Semaphore)
(define compile-place-semaphore (make-semaphore 1))

;; seashell-compile-files/place
;; Invokes seashell-compile-files in a separate place,
;; preserving parallelism in Racket.
(: seashell-compile-files/place (->* ((Listof String) (Listof String) Path (Listof Path) (Listof Path))
                                     (Nonnegative-Integer)
                                     (Values (U Bytes False) Seashell-Diagnostic-Table)))
(define (seashell-compile-files/place user-cflags user-ldflags question-dir sources objects [retries 5])
  ;; Set up the place (if it has died/never started)
  (seashell-compile-place/check-and-init)
  (define compiler-place (unbox global-compiler-place))
  (cond
    [(place? compiler-place)
      (define-values (read-end write-end) (place-channel))
      (define dead-evt (place-dead-evt compiler-place))
      (place-channel-put compiler-place
                         (list write-end user-cflags user-ldflags question-dir sources objects))
      (match (sync dead-evt (wrap-evt read-end deserialize))
        [(? (lambda (x) (eq? dead-evt x)))
         (cond
          [(retries . > . 0)
           (seashell-compile-files/place user-cflags user-ldflags question-dir sources objects (sub1 retries))]
          [else (raise (exn:fail (format "Compiler place dead!") (current-continuation-marks)))])]
        [(list exn? result data)
         (when exn?
               (raise (exn:fail (format "Exception raised in compiler place: ~a!" data)
                                (current-continuation-marks))))
         (values (cast result (U False Bytes))
                 (cast data Seashell-Diagnostic-Table))])]
    [else (raise (exn:fail (format "Compiler place dead!") (current-continuation-marks)))]))

;; seashell-compile-place/alive?
;; Tests if the Place is alive.
(: seashell-compile-place/alive? (-> Any))
(define (seashell-compile-place/alive?)
  (define compiler-place (unbox global-compiler-place))
  (and (place? compiler-place)
       (not (sync/timeout 0 (place-dead-evt compiler-place)))))

;; seashell-compile-place/shutdown
;; Terminates the place.
(: seashell-compile-place/shutdown (-> Any))
(define (seashell-compile-place/shutdown)
  (call-with-semaphore
    compile-place-semaphore
    (thunk
      (define compiler-place (unbox global-compiler-place))
      (cond
        [(and (place? compiler-place)
              (not (sync/timeout 0 (place-dead-evt compiler-place))))
         (logf 'debug "Shutting down compiler place...")
         (place-channel-put compiler-place 'quit)
         (place-wait compiler-place)]))))

;; seashell-compile-place/check-and-init
;; Sets up the place for compilation, if it hasn't started.
(: seashell-compile-place/check-and-init (-> Any))
(define (seashell-compile-place/check-and-init)
  (call-with-semaphore
    compile-place-semaphore
    (thunk
      (define compiler-place (unbox global-compiler-place))
      ;; seashell-compile-place/init
      ;; Sets up the place for compilation.
      (: seashell-compile-place/init (-> Any))
      (define (seashell-compile-place/init)
        ;; Set this up in the root custodian so we don't fail.
        (parameterize ([current-custodian (scheme_make_custodian #f)])
          ;; Make sure that we don't use current-output-port
          ;; or the detach will fail.
          (define-values (place in out err)
            (dynamic-place* 'seashell/compiler/place-main
                            'seashell-compiler-place
                            #:out (current-error-port)
                            #:err (current-error-port)))
          (assert (port? in))
          (set-box! global-compiler-place place)
          (close-output-port in)))
      (cond
        [(not compiler-place)
         (logf 'info "Starting up compiler place - first time.")
         (seashell-compile-place/init)]
        [(sync/timeout 0 (place-dead-evt compiler-place))
         (logf 'info "Restarting compiler place.")
         (seashell-compile-place/init)]
        [else #t]))))

