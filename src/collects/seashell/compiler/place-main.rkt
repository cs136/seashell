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
(module typed-place typed/racket
  (require seashell/compiler/compiler
           (submod seashell/seashell-config typed)
           seashell/log
           racket/place)
  (require/typed racket/serialize
                 [serialize (-> Any Any)])

  (provide seashell-compiler-place/typed)

  ;; (seashell-compiler-place/thread write-end . args)
  ;; Thread that actually processes the compilation request.
  (: seashell-compiler-place/thread (-> Place-Channel (Listof String) (Listof String) Path Path Thread))
  (define (seashell-compiler-place/thread write-end cflags ldflags question-dir source)
    (thread
      (lambda ()
        (with-handlers
          ([exn:fail?
            (lambda ([exn : exn]) (place-channel-put write-end (serialize (list #t #f (exn-message exn)))))])
          (define-values (result data)
            (seashell-compile-files cflags ldflags question-dir source))
          (place-channel-put write-end (serialize (list #f result data)))))))


  ;; (seashell-compiler-place channel)
  ;; Invokes seashell-compile-files in a separate place,
  ;; preserving parallelism in Racket.
  ;;
  ;; This is the main function for the place.
  (: seashell-compiler-place/typed (-> Place-Channel Any))
  (define (seashell-compiler-place/typed channel)
    ;; These two things should not fail.
    (config-refresh!)
    (standard-logger-setup)
    (let/ec quit : Any
      (let loop : Any ()
        (with-handlers
          ([exn:fail?
            (lambda ([exn : exn])
              (logf 'error "Compiler place got error: ~a~n" (exn-message exn))
              (place-channel-put channel (list #t #f (exn-message exn))))])
          (match (sync/timeout (read-config-nonnegative-real 'compiler-ttl) channel)
            [#f
             (logf 'info "Shutting down compiler place due to TTL expiry.")
             (quit #f)]
            ['quit
             (logf 'info "Shutting down compiler place due to request.")
             (quit #f)]
            [(list write-end cflags ldflags question-dir source)
             (assert (place-channel? write-end))
             (seashell-compiler-place/thread
               write-end
               (cast cflags (Listof String))
               (cast ldflags (Listof String))
               (cast question-dir Path)
               (cast source Path))])
          (loop))))))


(require (submod "." typed-place))
(provide seashell-compiler-place)
(define (seashell-compiler-place channel)
  (seashell-compiler-place/typed channel))
