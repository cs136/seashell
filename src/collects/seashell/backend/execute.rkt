;; Seashell
;; Copyright (C) 2012-2013 Jennifer Wong, Marc Burns
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
;;
;; Authors: Jennifer Wong, Marc Burns
(module seashell-run racket
  (require "log.rkt"
           "common.rkt"
           "seashell-config.rkt")
  (provide run-file
           kill-current-pgrm
           accept-user-input
           get-pgrm-output
           wait-pgrm)

  ;; Information associated with a running program.
  (define-struct pgrm (stdin stdout stderr ts handle reaped) #:mutable #:transparent)

  ;; Table of (session key -> pgrm) and associated semaphore.
  (define pgrm-table (make-hash))
  (define pgrm-sema (make-semaphore 1))

  ;; Thread to collect timed-out and otherwise dead programs.
  (define reaper
    (thread
     (thunk
      (let loop ()
        (sync (alarm-evt (+ (current-inexact-milliseconds) 1000)))
        (with-handlers
            ([exn? (lambda(e)
                     (logf 'info "Exception in reaper: ~a" e)
                     (loop))])
          (hash-map
           pgrm-table
           (lambda(k v)
             (when (and (not (pgrm-reaped v))
                        (or
                         (> (- (current-seconds)
                               (pgrm-ts v))
                            pgrm-idle-timeout)
                         (not (equal? ((pgrm-handle v) 'status)
                                      'running))))
               (set-pgrm-reaped! v #t)
               (thread
                (thunk
                 (logf 'info "Process for session=~a finished or timed out." k)
                 (when (equal? ((pgrm-handle v) 'status)
                               'running)
                   (pgrm-kill v))
                 ((pgrm-handle v) 'wait)
                 (sync/timeout 60 (eof-evt (pgrm-stdout v)))
                 (sync/timeout 60 (eof-evt (pgrm-stderr v)))
                 (semaphore-post pgrm-sema)
                 (hash-remove! pgrm-table k)
                 (semaphore-wait pgrm-sema)
                 (close-input-port (pgrm-stdout v))
                 (close-input-port (pgrm-stderr v))
                 (close-output-port (pgrm-stdin v))
                 (logf 'info "Reaped process for session=~a" k)))))))
        (loop)))))

  ;; Functions for starting/stopping programs.
  ;; (union (list file-name) string) -> pgrm
  (define (pgrm-start c-sources)
    ;; Allocate a temporary directory.
    (define workdir
      (make-temporary-file
       "seashell~a"
       'directory
       "/tmp"))
    ;; Get output binary name.
    (define binary-name
      (let ((f (make-temporary-file
                "exe~a"
                #f
                "/tmp")))
        (delete-file f)
        f))
    ;; Copy all sources to a working directory and
    ;; return a list of sources in the working directory.
    (define src-list
      (if (string? c-sources)
          (let ((source (build-path workdir "main.c")))
            (with-output-to-file source
              (thunk (display c-sources)))
            (list source))
          (map
           (lambda(file)
             (let*-values
                 (((clean-path) (simplify-path (string->path file) #t))
                  ((base name dir?) (split-path clean-path))
                  ((dest-file) (build-path workdir name)))
               (copy-file clean-path dest-file)
               dest-file))
           c-sources)))
    ;; Invoke test environment.
    (let
        ((proc
          (apply process*
                 `(,ce-helper-binary
                   ,binary-name
                   ,@src-list))))
      ;; Create a thread to clean up the working files
      ;; once the child process has returned.
      (thread
       (thunk
        ((fifth proc) 'wait)
        (delete-directory/files workdir)))
      (pgrm (second proc)
            (first proc)
            (fourth proc)
            (current-seconds)
            (fifth proc)
            #f)))

  (define (pgrm-kill pg)
    ((pgrm-handle pg) 'kill))

  ;; session key -> bool
  ;; True iff. session has a running program.
  (define (session-has-pgrm? key)
    (hash-has-key? pgrm-table key))

  ;; session key -> pgrm
  (define (get-session-pgrm key)
    (hash-ref pgrm-table key #f))

  ;; session key pgrm ->
  (define (set-session-pgrm key pg)
    (hash-set! pgrm-table key pg)
    (void))

  ;; filename -> bool
  (define (run-file key args uid)
    (match args
      [`(,(? string? src))
         ;; If a program is running, kill it.
         (semaphore-wait pgrm-sema)
         (when (session-has-pgrm? key)
           (pgrm-kill (get-session-pgrm key)))
         (semaphore-post pgrm-sema)
         ;; Start the new program. Currently, the exported run-file
         ;; routine accepts exactly one argument which is the source
         ;; of the program to run.
         (let ((new-pgrm (pgrm-start src)))
           (semaphore-wait pgrm-sema)
           (set-session-pgrm key new-pgrm)
           (semaphore-post pgrm-sema))
         #t]
      [else #f]))

  ;; -> bool
  (define (kill-current-pgrm key args uid)
    ;; If a program is running, kill it.
    (semaphore-wait pgrm-sema)
    (begin0
      (if (session-has-pgrm? key)
          (begin (pgrm-kill (get-session-pgrm key))
                 #t)
          #f)
      (semaphore-post pgrm-sema)))

  ;; string -> bool
  (define (accept-user-input key args uid)
    (match args
      [`(,(? string? inp))
       (let ((pg (hash-ref pgrm-table key #f)))
         (if (pgrm? pg)
             (begin
               (display inp (pgrm-stdin pg))
               #t)
             #f))]
      [else #f]))

  ;; -> (union string false)
  (define (get-pgrm-output key args uid)
    (let ((pg (hash-ref pgrm-table key #f))
          (b (make-bytes 256 0)))
      (if (and
           (pgrm? pg)
           (port?
            (sync/timeout 120 (pgrm-stdout pg))))
          (let ((rb (read-bytes-avail! b (pgrm-stdout pg))))
            (if (eof-object? rb)
                #f
                (bytes->string/utf-8 (subbytes b 0 rb))))
          #f)))

  ;; -> bool
  (define (wait-pgrm key args uid)
    (let ((pg (hash-ref pgrm-table key #f)))
      (if (pgrm? pg)
          (begin
            ((pgrm-handle pg) 'wait)
            (and
             (sync/timeout 60 (eof-evt (pgrm-stdout pg)))
             (sync/timeout 60 (eof-evt (pgrm-stderr pg)))
             #t))
          #f))))

