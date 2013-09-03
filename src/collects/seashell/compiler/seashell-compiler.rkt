#lang racket/base
;; Seashell's Clang interface.
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
(struct seashell-compiler (ptr))
(struct seashell-diagnostic (file line column message) #:transparent)

(require racket/contract
         racket/function
         racket/list
         racket/bool
         seashell/compiler/ffi)

(provide
  (contract-out
    [make-seashell-compiler
      (-> seashell-compiler?)]
    [seashell-compile-files
      (-> seashell-compiler?
          (listof string?)
          (listof string?)
          (listof path?)
          (values (or/c bytes? false?) (hash/c path? (listof seashell-diagnostic?))))])
    seashell-diagnostic-file
    seashell-diagnostic-line
    seashell-diagnostic-column
    seashell-diagnostic-message
    seashell-diagnostic?
    seashell-compiler?)

(define (seashell-compile-files compiler cflags ldflags sources)
  (define c (seashell-compiler-ptr compiler))
  (define file-vec (list->vector sources))
  (seashell_compiler_clear_files c)
  (seashell_compiler_clear_compile_flags c)
  (seashell_compiler_clear_link_flags c)
  (for-each ((curry seashell_compiler_add_compile_flag) c) cflags)
  (for-each ((curry seashell_compiler_add_link_flag) c) ldflags)
  (for-each ((curry seashell_compiler_add_file) c) (map path->string sources))
  (define res (seashell_compiler_run c))
  (define compiler-diags
    (foldl
      (lambda(c b) (hash-set b (car c) (cons (cdr c) (hash-ref b (car c) empty))))
      (make-immutable-hash)
      (for*/list ([i (in-range 0 (length sources))]
                  [j (in-range 0 (seashell_compiler_get_diagnostic_count c i))])
        `(,(vector-ref file-vec i) .
            ,(seashell-diagnostic (seashell_compiler_get_diagnostic_file c i j)
                                  (seashell_compiler_get_diagnostic_line c i j)
                                  (seashell_compiler_get_diagnostic_column c i j)
                                  (seashell_compiler_get_diagnostic_message c i j))))))
  (define linker-diags (seashell_compiler_get_linker_messages c))
  (define diags
    (if (equal? linker-diags "")
        compiler-diags
        (hash-set compiler-diags (string->path "a.out") (list (seashell-diagnostic "" 0 0 linker-diags)))))
  (if (= 0 res)
    (values (seashell_compiler_get_executable c) diags)
    (values #f diags)))

(define (make-seashell-compiler)
  (seashell-compiler (seashell_compiler_make)))
