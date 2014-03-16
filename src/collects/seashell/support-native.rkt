#lang racket 
;; Seashell's (native/OS dependant) support functions.
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

(provide seashell_drop_permissions
         seashell_signal_detach)

(define-ffi-definer define-support 
                    (ffi-lib (read-config 'seashell-support)))

(define-support seashell_drop_permissions (_fun -> _int))
(define-support seashell_signal_detach (_fun -> _void))
