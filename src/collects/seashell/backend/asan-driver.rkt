#lang typed/racket

;; This driver lets you run the asan-error-parse.rkt file
;; directly (ie outside of seashell). This is faster for
;; development because you do not have to rebuild anything.
;; To use this file, edit asan-error-parse.rkt by commenting out
;; the (require ...) for seashell-config and run this driver directly
;; from the terminal with racket, for example:
;; shell> racket asan-driver.rkt /path/to/asan_output.txt | python -m json.tool
;;
;; This file has no other use and is not used by Seashell
;; backend.

(require/typed "asan-error-parse.rkt"
               [asan->json (-> Bytes Path-String Bytes)])

(define stderr (current-error-port))

(define command-line-args (current-command-line-arguments))
(define asan-file (vector-ref command-line-args 0))
(fprintf stderr "DRIVER: Reading from ~a\n" asan-file)

(define file-contents (file->bytes asan-file))

(fprintf stderr "-------------- RESULT -----------------\n")

(printf "~a" (asan->json file-contents (path->string (build-path (find-system-path 'home-dir) ".seashell"))))
