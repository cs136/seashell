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
         seashell_create_secret_file
         seashell_uw_check_remote_user
         seashell_signal_detach)

(define-ffi-definer define-support 
                    (ffi-lib (read-config 'seashell-support)))

;; These functions return 0 on success and 1 on failure if they return anything.
;; Manually check the result of these functions - as failure can indicate there's
;; an underlying security issue that needs to be addressed.
(define-support seashell_drop_permissions (_fun -> _int))
(define-support seashell_signal_detach (_fun -> _int))
(define-support seashell_create_secret_file (_fun _path -> _int))
(define-support seashell_uw_check_remote_user (_fun -> _int))
