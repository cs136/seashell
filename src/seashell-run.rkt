(module seashell-run racket
  (require "log.rkt"
           "common.rkt"
           "config.rkt")
  (provide run-file
           kill-current-pgrm
           accept-user-input
           get-pgrm-output)
  
  ;; Information associated with a running program.
  (struct pgrm (stdin stdout stderr ts))
  
  ;; Table of session key -> pgrm and associated semaphore.
  (define pgrm-table (make-hash))
  (define pgrm-sema (make-semaphore 1))
  
  ;; filename -> bool
  (define (run-file args uid)
    #f)
  
  ;; -> bool
  (define (kill-current-pgrm args uid)
    #f)
  
  ;; string -> bool
  (define (accept-user-input args uid)
    #f)
  
  ;; -> (union string false)
  (define (get-pgrm-output args uid)
    #f))
  