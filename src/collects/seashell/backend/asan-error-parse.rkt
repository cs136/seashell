#lang typed/racket/no-check

(require "asan-error-message.rkt")
(require racket/port)

;; This is a binary that reads the asan error message
;; from the stdin and produces a racket-readable structure
;; to stdout.

;; Remove /no-check when developing and add it back before pushing for production.


(write (asan-error-parser (port->string)))
