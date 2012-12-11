#lang racket/base
;; Configuration file for Seashell server.
(provide (all-defined-out))

;; Host and port for web server.
(define seashell-webserver-host "127.0.0.1")
(define seashell-webserver-port 9876)

;; Database configuration.
(define seashell-db-file "seashell.db")
(define seashell-db-host "127.0.0.1")
(define seashell-db-port 11234)

;; Base path for file resources to serve during API calls.
(define res-root "http://seashell.convextech.ca/")

;; API session timeout in seconds.
(define api-session-timeout 3600)

;; Program idle timeout in seconds.
(define pgrm-idle-timeout 120)

;; Path of compile/execute helper binary.
(define ce-helper-binary "/ss_env")
