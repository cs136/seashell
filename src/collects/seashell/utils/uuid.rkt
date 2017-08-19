;; Typed Racket UUID bindings;
;; Copyright (C) 2017 The Seashell Maintainers
;; Adopted from mordae/racket-uuid; Copyright (C) 2014-2015 Jan Dvorak
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
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
#lang typed/racket
;
; FFI bindings for the libuuid library
;
(module ffi racket/base
  (require
    (rename-in ffi/unsafe (-> -->)))

  (require racket/contract
           ffi/unsafe/define)

  (provide uuid-generate
           uuid-generate/random
           uuid-generate/time)

  (provide
    (contract-out
      (uuid? predicate/c)))


  (define which-lib
    (case (system-type 'os)
      [(macosx) "libSystem"]
      [else "libuuid"]))

  (define-ffi-definer define-uuid (ffi-lib which-lib '("1" "")))


  (define (uuid? str)
    (and (string? str)
         (uuid-parse str)
         #t))

  (define-uuid uuid-parse
               (_fun _string/utf-8
                     (out : _bytes = (make-bytes 16))
                     --> (result : _int)
                     --> (and (= 0 result) out))
               #:c-id uuid_parse)

  (define-uuid uuid-generate
               (_fun (out : _bytes = (make-bytes 16))
                     --> _void
                     --> (uuid-unparse out))
               #:c-id uuid_generate)

  (define-uuid uuid-generate/random
               (_fun (out : _bytes = (make-bytes 16))
                     --> _void
                     --> (uuid-unparse out))
               #:c-id uuid_generate_random)

  (define-uuid uuid-generate/time
               (_fun (out : _bytes = (make-bytes 16))
                     --> _void
                     --> (uuid-unparse out))
               #:c-id uuid_generate_time)

  (define-uuid uuid-unparse
               (_fun (uuid : _bytes)
                     (out : _bytes = (make-bytes 32))
                     --> _void
                     --> (cast out _bytes _string/utf-8))
               #:c-id uuid_unparse))
(require/typed/provide (submod "." ffi)
  [uuid-generate (-> String)])

; vim:set ts=2 sw=2 et:
