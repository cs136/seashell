(module seashell-run racket
  (require "log.rkt"
           "common.rkt"
           "config.rkt")
  (provide run-file
           kill-current-pgrm
           accept-user-input
           get-pgrm-output)
  
  ;; Information associated with a running program.
  (struct pgrm (stdin stdout stderr ts handle))
  
  ;; Table of session key -> pgrm and associated semaphore.
  (define pgrm-table (make-hash))
  (define pgrm-sema (make-semaphore 1))
  
  ;; Thread to collect timed-out and otherwise dead programs.
  (define reaper
    (thread
     (thunk
      (let loop ()
        (sync (alarm-evt 1000))
        (semaphore-wait pgrm-sema)
        (with-handlers
            ([exn? (lambda(e)
                     (semaphore-post pgrm-sema)
                     (loop))])
          (hash-map
           (lambda(k v)
             (when (> (- (current-seconds)
                         (pgrm-ts v))
                      pgrm-idle-timeout)
               (pgrm-kill v)
               (hash-remove! pgrm-table k)))
           pgrm-table)
          (semaphore-post pgrm-sema))
        (loop)))))
                
  ;; Functions for starting/stopping programs.
  ;; (union (list file-name) string) -> pgrm
  (define (pgrm-start c-sources)
    ;; Allocate a temporary directory.
    (define workdir
      (make-temporary-file
       "seashell~a"
       'directory
       (find-system-path 'temp-dir)))
    ;; Get output binary name.
    (define binary-name
      (make-temporary-file "exe~a" #f workdir))
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
    (let-values
        (((stdout stdin pid stderr handle)
          (apply process*
                 `(,ce-helper-binary
                   ,ce-helper-binary
                   ,binary-name
                   ,@src-list))))
      ;; Create a thread to clean up the working files
      ;; once the child process has returned.
      (thread
       (thunk
        (handle 'wait)
        (delete-directory/files workdir)))
      (pgrm stdin stdout stderr (current-seconds) handle)))
  
  (define (pgrm-kill pg)
    ((pgrm-handle pg) 'kill))
  
  ;; filename -> bool
  (define (run-file key args uid)
    #f)
  
  ;; -> bool
  (define (kill-current-pgrm key args uid)
    #f)
  
  ;; string -> bool
  (define (accept-user-input key args uid)
    #f)
  
  ;; -> (union string false)
  (define (get-pgrm-output key args uid)
    #f))
  