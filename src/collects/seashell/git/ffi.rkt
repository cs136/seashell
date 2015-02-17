#lang racket

;; Seashell's libgit2 bindings.
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
(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         seashell/git/structs
         seashell/seashell-config)
(require (prefix-in contract: racket/contract))

(provide seashell_git_clone seashell_git_init
         seashell_git_commit_free seashell_git_commit_init
         seashell_git_commit_add seashell_git_commit
         seashell_git_commit_delete
         seashell_git_status_free
         seashell_git_get_status
         seashell_git_get_status/gc
         seashell_git_status_entrycount
         seashell_git_status_flags
         seashell_git_status_path)

;; Load the library (libseashell-git)
(define-ffi-definer define-git (ffi-lib (read-config 'seashell-git)))

;; Opaque data structure - make sure allocator/deallocator is set.
(define _seashell_git_update _pointer)
(define _seashell_git_status _pointer)

;; Error fetching function
(define-git seashell_git_error (_fun -> _string))

;; (check result function) -> any/c
;; Handles seashell-git errors correctly,
;; by raising a exn:git exception with the contents of seashell_git_error
(define (check result function)
  (unless (and result (or (not (number? result)) (zero? result)))
    (define message (format "~a: ~a" function (if (seashell_git_error) (seashell_git_error) "")))
    (raise (exn:git message (current-continuation-marks))))
  result)
;; Repository functions (init, clone)
(define-git seashell_git_clone (_fun _string _path -> (r : _int)
                                     -> (check r 'seashell_git_clone)))
(define-git seashell_git_init (_fun _path -> (r : _int)
                                    -> (check r 'seashell_git_init)))

;; Committing files (commit, add_file, ...)
(define-git seashell_git_commit_free (_fun _seashell_git_update -> _void)
            #:wrap (deallocator))
(define-git seashell_git_commit_init (_fun _path -> _seashell_git_update)
            #:wrap (allocator seashell_git_commit_free))
(define-git seashell_git_commit_add (_fun _seashell_git_update _path -> _void))
(define-git seashell_git_commit (_fun _seashell_git_update _string -> (r : _int)
                                     -> (check r 'seashell_git_commit)))
(define-git seashell_git_commit_delete (_fun _seashell_git_update _path -> _void))

;; Status functions.
(define-git seashell_git_status_free (_fun _seashell_git_status -> _void)
            #:wrap (deallocator))
;; XXX Manually wrap seashell_git_get_status with a (allocator seashell_git_status_free)
;; We'd rather not have strange FFI/place interactions here.
(define-git seashell_git_get_status (_fun _path -> (r : _seashell_git_status)
                                          -> (check r 'seashell_git_get_status)))
(define seashell_git_get_status/gc ((allocator seashell_git_status_free) seashell_git_get_status))
(define-git seashell_git_status_entrycount (_fun _seashell_git_status -> _size))
(define-git seashell_git_status_flags (_fun _seashell_git_status _size -> _int))
(define-git seashell_git_status_path (_fun _seashell_git_status _size -> _path))
