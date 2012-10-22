#lang racket/base
;; Configuration file for Seashell server.
(provide (all-defined-out))

;; Host and port for web server.
(define seashell-webserver-host "127.0.0.1")
(define seashell-webserver-port 9876)

;; Base path for file resources to serve during API calls.
(define res-root "http://seashell.convextech.ca/")

