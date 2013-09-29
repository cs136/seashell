#lang racket/base
;; Seashell's LLVM/Clang interface FFI bindings.
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

;; This file contains the run-once-per-process initialization code
;; for the LLVM/Clang FFI interface.  Do not explicitly require
;; this file.
(require ffi/unsafe 
         ffi/unsafe/custodian
         seashell/compiler/ffi)

;; Start up and cleanup procedures.
(void
  (register-custodian-shutdown
    (void)
    (lambda(v) (seashell_llvm_cleanup))
    (current-custodian)
    #:at-exit? #t))
(void (seashell_llvm_setup))
