#lang typed/racket/base
;; Seashell's LLVM/Clang interface FFI bindings.
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
(module untyped racket/base
  (require ffi/unsafe
           ffi/unsafe/define
           ffi/unsafe/alloc
           seashell/seashell-config)

  ;; Exported FFI functions.  See compiler.cc for more details.
  (provide seashell_compiler_free
           seashell_compiler_make
           seashell_compiler_add_file
           seashell_compiler_clear_files
           seashell_compiler_add_compile_flag
           seashell_compiler_clear_compile_flags
           seashell_compiler_get_linker_messages
           seashell_compiler_get_diagnostic_count
           seashell_compiler_get_diagnostic_line
           seashell_compiler_get_diagnostic_column
           seashell_compiler_get_diagnostic_error
           seashell_compiler_get_diagnostic_file
           seashell_compiler_get_diagnostic_message
           seashell_compiler_run
           seashell_compiler_get_object
           seashell_clang_version
           seashell_compiler_object_arch
           seashell_compiler_object_os
           seashell_compiler-ptr?)

  (define-ffi-definer define-clang
                      (ffi-lib (read-config 'seashell-clang)))
  (define-cpointer-type _seashell_compiler-ptr)
  (define-clang seashell_clang_version (_fun -> _string))
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
  (define-clang seashell_compiler_get_linker_messages
                (_fun _seashell_compiler-ptr -> _string/utf-8))
  (define-clang seashell_compiler_get_diagnostic_count
                (_fun _seashell_compiler-ptr _int -> _int))
  (define-clang seashell_compiler_get_diagnostic_line
                (_fun _seashell_compiler-ptr _int _int -> _int))
  (define-clang seashell_compiler_get_diagnostic_error
                (_fun _seashell_compiler-ptr _int _int -> _bool))
  (define-clang seashell_compiler_get_diagnostic_column
                (_fun _seashell_compiler-ptr _int _int -> _int))
  (define-clang seashell_compiler_get_diagnostic_file
                (_fun _seashell_compiler-ptr _int _int -> _string/utf-8))
  (define-clang seashell_compiler_get_diagnostic_message
                (_fun _seashell_compiler-ptr _int _int -> _string/utf-8))
  (define-clang seashell_compiler_run
                (_fun _seashell_compiler-ptr -> _int))
  (define-clang seashell_compiler_object_arch
                (_fun _seashell_compiler-ptr -> _string))
  (define-clang seashell_compiler_object_os
                (_fun _seashell_compiler-ptr -> _string))
  (define-clang seashell_compiler_get_object
                (_fun _seashell_compiler-ptr (o : (_ptr o _int)) -> (r : _pointer) -> (values o r))
                #:wrap
                (lambda (proc)
                  (lambda (comp)
                    (let-values ([(size address) (proc comp)])
                      (cond
                        [address
                          (define result (make-bytes size))
                          (memcpy result address size)
                          result]
                        [else #f]))))))
(require/typed (submod "." untyped)
               [#:opaque Seashell-Compiler-Ptr seashell_compiler-ptr?]
               [seashell_clang_version (-> String)]
               [seashell_compiler_free (-> Seashell-Compiler-Ptr Void)]
               [seashell_compiler_make (-> Seashell-Compiler-Ptr)]
               [seashell_compiler_add_file (-> Seashell-Compiler-Ptr String Void)]
               [seashell_compiler_clear_files (-> Seashell-Compiler-Ptr Void)]
               [seashell_compiler_add_compile_flag (-> Seashell-Compiler-Ptr String Void)]
               [seashell_compiler_clear_compile_flags (-> Seashell-Compiler-Ptr Void)]
               [seashell_compiler_get_linker_messages (-> Seashell-Compiler-Ptr String)]
               [seashell_compiler_get_diagnostic_count (-> Seashell-Compiler-Ptr Nonnegative-Integer Nonnegative-Integer)]
               [seashell_compiler_get_diagnostic_line (-> Seashell-Compiler-Ptr Nonnegative-Integer Nonnegative-Integer Nonnegative-Integer)]
               [seashell_compiler_get_diagnostic_error (-> Seashell-Compiler-Ptr Nonnegative-Integer Nonnegative-Integer Boolean)]
               [seashell_compiler_get_diagnostic_column (-> Seashell-Compiler-Ptr Nonnegative-Integer Nonnegative-Integer Nonnegative-Integer)]
               [seashell_compiler_get_diagnostic_file (-> Seashell-Compiler-Ptr Nonnegative-Integer Nonnegative-Integer String)]
               [seashell_compiler_get_diagnostic_message (-> Seashell-Compiler-Ptr Nonnegative-Integer Nonnegative-Integer String)]
               [seashell_compiler_run (-> Seashell-Compiler-Ptr Integer)]
               [seashell_compiler_object_arch (-> Seashell-Compiler-Ptr String)]
               [seashell_compiler_object_os (-> Seashell-Compiler-Ptr String)]
               [seashell_compiler_get_object (-> Seashell-Compiler-Ptr (U Bytes False))])
  (provide seashell_compiler_free
           seashell_compiler_make
           seashell_compiler_add_file
           seashell_compiler_clear_files
           seashell_compiler_add_compile_flag
           seashell_compiler_clear_compile_flags
           seashell_compiler_get_linker_messages
           seashell_compiler_get_diagnostic_count
           seashell_compiler_get_diagnostic_line
           seashell_compiler_get_diagnostic_column
           seashell_compiler_get_diagnostic_error
           seashell_compiler_get_diagnostic_file
           seashell_compiler_get_diagnostic_message
           seashell_compiler_run
           seashell_compiler_get_object
           seashell_clang_version
           seashell_compiler_object_arch
           seashell_compiler_object_os
           seashell_compiler-ptr?)
