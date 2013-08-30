#!/usr/bin/racket
#lang racket
;; Seashell's backend server.
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
(struct exn:seashell-compiler exn:fail:user ())
(struct seashell-compiler (ptr))
(struct seashell-diagnostic (file line column message) #:transparent)

(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         ffi/unsafe/custodian
         (rename-in racket/contract (-> ->/c)))

(provide make-seashell-compiler
         seashell-compile-files
         seashell-diagnostic)

(define-ffi-definer define-clang (ffi-lib "libseashell-clang"))

(define _seashell_compiler-ptr (_cpointer 'seashell_compiler))

(define-clang seashell_llvm_setup (_fun -> _void))

(define-clang seashell_llvm_cleanup (_fun -> _void))

(define-clang seashell_compiler_free (_fun _seashell_compiler-ptr -> _void)
              #:wrap (deallocator))

(define-clang seashell_compiler_make (_fun -> _seashell_compiler-ptr)
              #:wrap (allocator seashell_compiler_free))

(define-clang seashell_compiler_add_file
              (_fun _seashell_compiler-ptr _string/utf-8 -> _void))

(define-clang seashell_compiler_clear_files
              (_fun _seashell_compiler-ptr -> _void))

(define-clang seashell_compiler_add_compile_flag
              (_fun _seashell_compiler-ptr _string/utf-8 -> _void))

(define-clang seashell_compiler_clear_compile_flags
              (_fun _seashell_compiler-ptr -> _void))

(define-clang seashell_compiler_add_link_flag
              (_fun _seashell_compiler-ptr _string/utf-8 -> _void))

(define-clang seashell_compiler_clear_link_flags
              (_fun _seashell_compiler-ptr -> _void))

(define-clang seashell_compiler_get_linker_messages
              (_fun _seashell_compiler-ptr -> _string/utf-8))

(define-clang seashell_compiler_get_diagnostic_count
              (_fun _seashell_compiler-ptr _int -> _int))

(define-clang seashell_compiler_get_diagnostic_line
              (_fun _seashell_compiler-ptr _int _int -> _int))

(define-clang seashell_compiler_get_diagnostic_column
              (_fun _seashell_compiler-ptr _int _int -> _int))

(define-clang seashell_compiler_get_diagnostic_file
              (_fun _seashell_compiler-ptr _int _int -> _string/utf-8))

(define-clang seashell_compiler_get_diagnostic_message
              (_fun _seashell_compiler-ptr _int _int -> _string/utf-8))

(define-clang seashell_compiler_run
              (_fun _seashell_compiler-ptr -> _int))

(define-clang seashell_compiler_get_executable
              (_fun _seashell_compiler-ptr (o : (_ptr o _int)) -> (r : _pointer) -> (values o r))
              #:wrap
              (lambda(proc)
                (lambda(comp)
                  (let-values (((size address) (proc comp)))
                    (if address
                      (make-sized-byte-string
                        (malloc size _bytes address 'nonatomic)
                        size)
                      #f)))))

(define/contract (seashell-compile-files compiler cflags ldflags sources)
  (->/c seashell-compiler?
        (listof string?)
        (listof string?)
        (listof path?)
        (values (or/c bytes? false?) (hash/c path? (listof seashell-diagnostic?))))
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

(define/contract (make-seashell-compiler)
  (->/c seashell-compiler?)
  (seashell-compiler (seashell_compiler_make)))

(void
  (register-custodian-shutdown
    (void)
    (lambda(v) (seashell_llvm_cleanup))
    (current-custodian)
    #:at-exit? #t))

(void (seashell_llvm_setup))
