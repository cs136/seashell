#lang typed/racket
;; Seashell
;; Copyright (C) 2016 The Seashell Maintainers
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

(provide acquire-exclusive-write-lock
         release-exclusive-write-lock
         call-with-exclusive-write-lock
         acquire-write-lock
         release-write-lock
         call-with-write-lock)

;; Some arbitrary number of concurrent writes to block at.
;; We should probably allow a couple of concurrent writes if students have
;; different windows open.. more than a few wouldn't be useful though
(define concurrent-writes 10)
(define write-sem (make-semaphore concurrent-writes))

;; (acquire-exclusive-write-lock)
;; Takes an exclusive write lock on the entire projects directory.
;; Used to sync offline changes.
(: acquire-exclusive-write-lock (-> Void))
(define (acquire-exclusive-write-lock)
  (for ([i (in-range 1 concurrent-writes)])
    (semaphore-wait write-sem)))

;; (release-exclusive-write-lock)
;; Releases the exclusive lock on the entire project directory.
;; Will throw an exception if the lock was not held.
(: release-exclusive-write-lock (-> Void))
(define (release-exclusive-write-lock)
  (for ([i (in-range 1 concurrent-writes)])
    (semaphore-post write-sem)))

;; (call-with-write-lock thunk)
;; Calls the given thunk with an exclusive write lock
(: call-with-exclusive-write-lock (All (A) (-> (-> A) A)))
(define (call-with-exclusive-write-lock thunk)
  (dynamic-wind
    acquire-exclusive-write-lock
    thunk
    release-exclusive-write-lock))

;; (acquire-write-lock)
;; Acquires a semaphore to write to the projects directory.
;; Mainly used to prevent offline changes from being synced
;; while changes are being made to the filesystem.
(: acquire-write-lock (-> Void))
(define (acquire-write-lock)
  (semaphore-wait write-sem))

;; (release-write-lock)
;; Releases the above semaphore.
(: release-write-lock (-> Void))
(define (release-write-lock)
  (semaphore-post write-sem))

;; (call-with-write-lock thunk)
;; Calls the given thunk with the write semaphore.
(: call-with-write-lock (All (A) (-> (-> A) A)))
(define (call-with-write-lock thunk)
  (dynamic-wind
    acquire-write-lock
    thunk
    release-write-lock))
