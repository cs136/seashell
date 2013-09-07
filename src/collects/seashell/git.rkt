#lang racket
;; Seashell's libgit2 bindings.
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
(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         racket/runtime-path)

;; Load the library (libseashell-git)
(define-ffi-definer define-git (ffi-lib (read-config 'seashell-git)))

;; Opaque data structure - make sure allocator/deallocator is set.
(define _seashell_git_update (_cpointer 'seashell_git_update*))

;; Exception type.
(struct exn:git exn:fail ())

;; Error fetching function
(define-git seashell_git_error (_fun -> _string))

;; (check result function) -> void?
;; Handles seashell-git errors correctly,
;; by raising a exn:git exception with the contents of seashell_git_error
(define (check result function)
  (unless (zero? result)
    (define message (format "~a: ~a" function (if (seashell_git_error) (seashell_git_error) "")))
    (raise (exn:git message (current-continuation-marks)))))

;; Repository functions (init, clone)
(define-git seashell_git_clone (_fun _string _string -> (r : _int)
                                     -> (check r 'seashell_git_clone)))
(define-git seashell_git_init (_fun _string -> (r : _int)
                                    -> (check r 'seashell_git_init)))

;; Committing files (commit, add_file, ...)
(define-git seashell_git_commit_free (_fun _seashell_git_update -> _void)
            #:wrap (deallocator))
(define-git seashell_git_commit_init (_fun _string -> _seashell_git_update)
            #:wrap (allocator seashell_git_commit_free))
(define-git seashell_git_commit_add (_fun _seashell_git_update _string -> _void))
(define-git seashell_git_commit (_fun _seashell_git_update -> (r : _int)
                                     -> (check r 'seashell_git_commit)))
(define-git seashell_git_commit_delete (_fun _seashell_git_update _string -> _void))

;; Provided wrapper functions
;; See git.cc for documentation.
(define seashell-git-init seashell_git_init)
(define seashell-git-clone seashell_git_clone)
(define seashell-git-update? _seashell_git_update?)
(define seashell-git-make-commit seashell_git_commit_init)
(define seashell-git-commit-add-file seashell_git_commit_add)
(define seashell-git-commit-delete-file seashell_git_commit_delete)
(define seashell-git-commit seashell-git_commit)

(provide exn:git? seashell-git-init seashell-git-clone seashell-git-update? seashell-git-make-commit
         seashell-git-commit-add-file seashell-git-commit-delete-file seashell-git-commit)
