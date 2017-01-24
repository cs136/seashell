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

(require/typed ffi/unsafe/atomic
               [start-atomic (-> Void)]
               [end-atomic (-> Void)])

(provide call-with-exclusive-write-lock
         call-with-write-lock)

;; Some arbitrary number of concurrent writes to block at.
;; We should probably allow a couple of concurrent writes if students have
;; different windows open.. more than a few wouldn't be useful though
(define concurrent-writes 10)

;; Two custom synchronization primitves are defined below.
;; The safe-lock primitive is a general-purpose lock which detects dead threads holding locks.
;; The safe-semaphore is not as general. It enforces that you must wait for a semaphore
;; before posting to it. This is to easily allow checking for unbalances caused by dead
;; threads. Keep this in mind before using it for something new.
(struct safe-semaphore ([count : Integer] [threads : (Listof Thread)]) #:mutable)
(struct safe-lock ([thread : (U Thread False)]) #:mutable)

(: make-safe-semaphore (-> Integer safe-semaphore))
(define (make-safe-semaphore count)
  (safe-semaphore count '()))

(: safe-semaphore-post (-> safe-semaphore Void))
(define (safe-semaphore-post sem)
  (start-atomic)
  (cond
    [(member (current-thread) (safe-semaphore-threads sem))
     (set-safe-semaphore-count! sem (+ 1 (safe-semaphore-count sem)))
     (set-safe-semaphore-threads! sem (remove (current-thread) (safe-semaphore-threads sem)))]
    [else (end-atomic) (error "Unbalanced safe-semaphore post before wait")])
  (end-atomic))

(: safe-semaphore-wait (-> safe-semaphore Void))
(define (safe-semaphore-wait sem)
  (start-atomic)
  (define dead-thds (filter thread-dead? (safe-semaphore-threads sem)))
  (set-safe-semaphore-count! sem (- (safe-semaphore-count sem) (length dead-thds)))
  (set-safe-semaphore-threads! sem (remove* dead-thds (safe-semaphore-threads sem)))
  (cond
    [(< 0 (safe-semaphore-count sem))
     (set-safe-semaphore-count! sem (- (safe-semaphore-count sem) 1))
     (set-safe-semaphore-threads! sem (cons (current-thread) (safe-semaphore-threads sem)))
     (end-atomic)]
    [else (end-atomic) (sleep 0) (safe-semaphore-wait sem)]))

(: make-safe-lock (-> safe-lock))
(define (make-safe-lock)
  (safe-lock #f))

(: safe-lock-acquire (-> safe-lock Void))
(define (safe-lock-acquire lock)
  (start-atomic)
  (define thd (safe-lock-thread lock))
  (cond
    [(eq? (current-thread) thd) (end-atomic)]
    [(or (not thd)
         (and thd (thread-dead? thd)))
     (set-safe-lock-thread! lock (current-thread))
     (end-atomic)]
    [else (end-atomic) (sleep 0) (safe-lock-acquire lock)]))

(: safe-lock-release (-> safe-lock Void))
(define (safe-lock-release lock)
  (start-atomic)
  (cond
    [(eq? (current-thread) (safe-lock-thread lock))
     (set-safe-lock-thread! lock #f)]
    [else (void)])
  (end-atomic))

(: has-safe-lock? (-> safe-lock Boolean))
(define (has-safe-lock? lock)
  (eq? (current-thread) (safe-lock-thread lock)))

;; End of sync primitives code

(define write-sem (make-safe-semaphore concurrent-writes))
(define excl-lock (make-safe-lock))

;; (acquire-exclusive-write-lock)
;; Takes an exclusive write lock on the entire projects directory.
;; Used to sync offline changes.
(: acquire-exclusive-write-lock (-> Void))
(define (acquire-exclusive-write-lock)
  (safe-lock-acquire excl-lock)
  (for ([i (in-range concurrent-writes)])
    (safe-semaphore-wait write-sem)))

;; (release-exclusive-write-lock)
;; Releases the exclusive lock on the entire project directory.
;; Will throw an exception if the lock was not held.
(: release-exclusive-write-lock (-> Void))
(define (release-exclusive-write-lock)
  (for ([i (in-range concurrent-writes)])
    (safe-semaphore-post write-sem))
  (safe-lock-release excl-lock))

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
  (unless (has-safe-lock? excl-lock)
    (safe-semaphore-wait write-sem)))

;; (release-write-lock)
;; Releases the above semaphore.
(: release-write-lock (-> Void))
(define (release-write-lock)
  (unless (has-safe-lock? excl-lock)
    (safe-semaphore-post write-sem)))

;; (call-with-write-lock thunk)
;; Calls the given thunk with the write semaphore.
(: call-with-write-lock (All (A) (-> (-> A) A)))
(define (call-with-write-lock thunk)
  (dynamic-wind
    acquire-write-lock
    thunk
    release-write-lock))
