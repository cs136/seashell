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
         seashell/seashell-config
         seashell/log
         seashell/git/git)
(require (prefix-in contract: racket/contract))
(provide seashell-git-place)

;; Entry point for the thread that handles disk-intensive
;; Seashell git operations.
(define (seashell-git-place channel)
  (config-refresh!)
  (standard-logger-setup)
  ;; Exceptions are currently passed through the channel.
  (let loop ()
    (with-handlers
        ([exn:fail? (lambda (exn) (place-channel-put channel (list #t (exn-message exn))))])
      (match (place-channel-get channel)
             [`(seashell-git-get-status/raw ,repo)
               (place-channel-put channel (list #f (seashell-git-get-status/raw repo)))]
             [`(seashell-git-init ,path)
               (place-channel-put channel (list #f (seashell-git-init path)))]
             [`(seashell-git-clone ,from ,to)
               (place-channel-put channel (list #f (seashell-git-clone from to)))]
             [`(seashell-git-commit ,update ,message)
               (place-channel-put channel (list #f (seashell-git-commit update message)))])
      (loop))))

