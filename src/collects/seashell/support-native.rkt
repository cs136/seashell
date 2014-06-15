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
         seashell_get_username
         seashell_set_umask
         seashell_signal_detach
         try-and-lock-file)

(define-ffi-definer define-support 
                    (ffi-lib (read-config 'seashell-support)))
(define-ffi-definer define-self 
                    (ffi-lib #f))

;; These functions return 0 on success and 1 on failure if they return anything.
;; Manually check the result of these functions - as failure can indicate there's
;; an underlying security issue that needs to be addressed.
(define-support seashell_drop_permissions (_fun -> _int))
(define-support seashell_set_umask (_fun -> _void))
(define-support seashell_signal_detach (_fun -> _int))
(define-support seashell_create_secret_file (_fun _path -> _int))
(define-support seashell_uw_check_remote_user (_fun -> _int))
(define-support seashell_get_username (_fun -> _string))
(define-support seashell_try_and_lock_file (_fun _int -> _int))

;; Underlying Racket support functions.
(define-self scheme_get_port_file_descriptor
             (_fun (port : _scheme) (fd : (_ptr o _long)) -> (not-error? : _bool) -> (values fd not-error?)))

;; try-and-lock-file port? -> bool?
;; Attempts to lock the file @ port using fcntl, returning #f if it could not,
;; #t otherwise.
(define (try-and-lock-file port)
  (define-values (fd not-error?) (scheme_get_port_file_descriptor port))
  (cond
    [not-error? (= 0 (seashell_try_and_lock_file fd))]
    [else #f]))
