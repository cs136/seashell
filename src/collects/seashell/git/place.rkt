#lang racket
;; Seashell's libgit2 bindings.
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
(define git-place-lock (make-semaphore 1))
(define git-place #f)

(require ffi/unsafe/alloc)
(require seashell/git/ffi)
(require seashell/git/git)
(require seashell/git/structs)

(provide seashell-git-place/init
         seashell-git-get-status/place
         seashell-git-init/place
         seashell-git-clone/place
         seashell-git-commit/place)

;; (seashell-git-place/init)
;; Sets up the Seashell git place.
(define (seashell-git-place/init)
  ;; Make sure we don't use current-output-port or the detach
  ;; will fail.
  (define-values (place in out err)
    (dynamic-place* 'seashell/git/place-main
                    'seashell-git-place
                    #:out (current-error-port)
                    #:err (current-error-port)))
  (set! git-place place))

;; (seashell-git-get-status/place repo) -> seashell-git-status?
;; Gets the status of a repository.
;;
;; Arguments:
;;  repo - Full path to the repository.
;; Returns:
;;  A seashell-git-status? structure
(define wrap-git-gc ((allocator seashell_git_status_free) values))
(define/contract (seashell-git-get-status/place repo)
  (-> path? seashell-git-status?)
  (call-with-semaphore
    git-place-lock
    (thunk
      (match-define (list exn? data)
                    (place-channel-put/get git-place `(seashell-git-get-status/raw ,repo)))
      (when exn?
        (raise (exn:git data (current-continuation-marks))))
      (seashell-git-status (wrap-git-gc data)))))

;; (seashell-git-init/place repo)?
;; Creates a new repository.
;;
;; Arguments:
;;  repo - Full path to the repository.
(define/contract (seashell-git-init/place repo)
  (-> path? any/c)
  (call-with-semaphore
    git-place-lock
    (thunk
      (match-define (list exn? data)
                    (place-channel-put/get git-place `(seashell-git-init ,repo)))
      (when exn?
        (raise (exn:git data (current-continuation-marks))))
      (void))))

;; (seashell-git-clone/place from to)?
;; Creates a new repository.
;;
;; Arguments:
;;  From - Location to clone from.
;;  To   - Path to clone to.
(define/contract (seashell-git-clone/place from to)
  (-> string? path? any/c)
  (call-with-semaphore
    git-place-lock
    (thunk (copy-directory/files from to))))
    #|
    (thunk
      (match-define (list exn? data)
                    (place-channel-put/get git-place `(seashell-git-clone ,from ,to)))
      (when exn?
        (raise (exn:git data (current-continuation-marks))))
      (void))))
    |#

;; (seashell-git-commit/place update message)
;; Commits an update.
;;
;; Arguments:
;;  update - Update to commit.
;;  message - Message to commit.
(define/contract (seashell-git-commit/place update message)
  (-> seashell-git-update? string? any/c)
  (call-with-semaphore
    git-place-lock
    (thunk
      (match-define (list exn? data)
                    (place-channel-put/get git-place `(seashell-git-update ,update ,message)))
      (when exn?
        (raise (exn:git data (current-continuation-marks))))
      (void))))
