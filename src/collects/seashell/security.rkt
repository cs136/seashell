#lang racket 
;; Seashell's security helpers.
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
(require ffi/unsafe
         ffi/unsafe/define)
(require racket/runtime-path)
(require seashell/seashell-config)

(provide seashell_drop_permissions)

(define-logger crypto)
(struct exn:crypto exn:fail:user ())

;; Load the crypto library
(define-ffi-definer define-crypto 
                    (ffi-lib (read-config 'seashell-security)))

;; Setup and Error functions
(define-crypto seashell_drop_permissions (_fun -> _int))
