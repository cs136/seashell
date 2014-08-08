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
(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         racket/runtime-path
         seashell/git/ffi
         seashell/git/structs
         seashell/seashell-config)
(require (prefix-in contract: racket/contract))
;; (seashell-git-get-status/raw repo)
;; Gets the status of a repository.  Returns a pointer which
;; is not stored in the Garbage Collector.
;; 
;; Arguments:
;;  repo - Full path to repository.
;; Returns:
;;  A _cpointer? pointing to the status structure.
(define/contract (seashell-git-get-status/raw repo)
  (contract:-> path? cpointer?)
  (seashell_git_get_status repo))

;; (seashell-git-get-status repo)
;; Gets the status of a repository.
;;
;; Arguments:
;;  repo - Full path to the repository.
;; Returns:
;;  A seashell-git-status? structure
(define/contract (seashell-git-get-status repo)
  (contract:-> path? seashell-git-status?)
  (seashell-git-status (seashell_git_get_status/gc repo)))

;; (seashell-git-status-entrycount status)
;; Gets the number of entries in a Seashell GIT
;; status structure.
;;
;; Arguments:
;;  status - A seashell-git-status? structure.
;; Returns:
;;  Number of entries in the structure.
(define/contract (seashell-git-status-entrycount status)
  (contract:-> seashell-git-status? integer?)
  (seashell_git_status_entrycount (seashell-git-status-status status)))

;; (seashell-git-status-flags status index)
;; Gets the flags associated with a status entry at index.
;;
;; Arguments:
;;  status - Seashell GIT status structure.
;; Returns:
;;  Flags associated with the entry.
(define/contract (seashell-git-status-flags status index)
  (contract:->i 
    ([status seashell-git-status]
     [index (status) (integer-in 0 (sub1 (seashell-git-status-entrycount status)))])
    [result integer?])
  (seashell_git_status_flags (seashell-git-status-status status) index))

;; (seashell-git-status-path status index)
;; Gets the [relative] path associated with a status entry at index.
;;
;; Arguments:
;;  status - Seashell GIT status structure.
;; Returns:
;;  Path associated with the entry.
(define/contract (seashell-git-status-path status index)
  (contract:->i 
    ([status seashell-git-status]
     [index (status) (integer-in 0 (sub1 (seashell-git-status-entrycount status)))])
    [result path?])
  (seashell_git_status_path (seashell-git-status-status status) index))


;; (seashell-git-init path)
;; Creates a new repository at path.
;;
;; Arguments:
;;  path - Full path 
(define/contract (seashell-git-init path)
  (contract:-> path? any/c)
  (seashell_git_init path))

;; (seashell-git-clone from to)
;; Clones a repository.
;;
;; Arguments:
;;  from - From
;;  to - To
(define/contract (seashell-git-clone from to)
  (contract:-> string? path? any/c)
  (seashell_git_clone from to))

;; (seashell-git-make-commit repo) -> seashell-git-update?
;; Creates a new commit update object acting on repo.
;;
;; Arguments:
;;  repo - Seashell git repository path.
;; Returns:
;;  A seashell-git-update? object that can be used to batch updates.
(define/contract (seashell-git-make-commit repo)
  (contract:-> path? seashell-git-update?)
  (seashell-git-update (seashell_git_commit_init repo)))

;; (seashell-git-commit-add-file update file)
;; Adds a file to the git commit object.
;; 
;; Arguments:
;;  update - Seashell git commit update.
;;  file - File to add.
(define/contract (seashell-git-commit-add-file update file)
  (contract:-> seashell-git-update? path? any/c)
  (seashell_git_commit_add (seashell-git-update-update update) file))

;; (seashell-git-commit-delete-file update file)
;; Deletes a file to the git commit object.
;; 
;; Arguments:
;;  update - Seashell git commit update.
;;  file - File to delete.
(define/contract (seashell-git-commit-delete-file update file)
  (contract:-> seashell-git-update? path? any/c)
  (seashell_git_commit_delete (seashell-git-update-update update) file))

;; (seashell-git-commit update message)
;; Commits an update.
;;
;; Arguments:
;;  update - Update to commit.
;;  message - Message to commit.
(define/contract (seashell-git-commit update message)
  (contract:-> seashell-git-update? string? any/c)
  (seashell_git_commit (seashell-git-update-update update) message))

;; Flag test functions.  Make sure these
;; remain consistent with libgit2.
(define (seashell-git-flag-new? flags)
  (bitwise-bit-set? flags 7))
(define (seashell-git-flag-modified? flags)
  (bitwise-bit-set? flags 8))
(define (seashell-git-flag-deleted? flags)
  (bitwise-bit-set? flags 9))


(provide seashell-git-init seashell-git-clone seashell-git-make-commit
         seashell-git-commit-add-file seashell-git-commit-delete-file seashell-git-commit
         seashell-git-update?
         (struct-out seashell-git-update))
(provide (struct-out seashell-git-status)
         seashell-git-get-status/raw
         seashell-git-get-status seashell-git-status-entrycount seashell-git-status-path seashell-git-status-flags)
(provide seashell-git-flag-new?
         seashell-git-flag-modified?
         seashell-git-flag-deleted?)
