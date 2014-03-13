#lang racket
;; Seashell's backend server.
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
(require net/url
         racket/async-channel
         racket/serialize
         seashell/websocket
         seashell/log
         seashell/seashell-config
         seashell/security
         seashell/overrides/ssl-unit-tcp
         seashell/backend/dispatch
         seashell/backend/project
         seashell/backend/http-dispatchers
         seashell/backend/authenticate
         seashell/crypto
         web-server/web-server
         web-server/http/xexpr
         web-server/http/request-structs
         web-server/dispatchers/dispatch
         (prefix-in sequence: web-server/dispatchers/dispatch-sequencer)
         (prefix-in filter: web-server/dispatchers/dispatch-filter))
         

(provide backend-main)

;; Channel used to keep process alive.
(define keepalive-chan (make-async-channel))

;; init-environment -> void?
;; Sets up the Seashell project environment
(define/contract (init-environment)
  (-> void?)
  (when (not (directory-exists? (read-config 'seashell)))
    (make-directory (read-config 'seashell))))

;; (backend/main)
;; Entry point to the backend server.
;;
;; This function is invoked directly from login-process.c
(define (backend-main)
  ;; SSL setup.
  ;; TODO: DHE keys.
  (define ssl-unit
    (make-ssl-tcp@
     (read-config 'ssl-cert)
     (read-config 'ssl-key)
     #f #f #f #f #f))
  
  ;; Dropping permissions.
  (unless (= 0 (seashell_drop_permissions))
    (fprintf (current-error-port) "Failed to drop permissions!  Exiting...~n")
    (exit 1))
  
  (with-handlers
    ([exn? (lambda (exn)
             (fprintf (current-error-port) "Exception raised in startup code: ~a~n" (exn-message exn))
             (exit 1))])

    ;; Directory setup.
    (init-environment)
    (init-projects)

    ;; Log / handlers setup.
    (current-error-port (open-output-file (build-path (read-config 'seashell) "seashell.log")
                                          #:exists 'append))
    (standard-logger-setup))

  (with-handlers
    ([exn? (lambda (exn)
             (logf 'error "Exception raised in Seashell!  Exiting...")
             ;; TODO: cleanup code
             (exit 1))])
  
    (logf 'info "Starting up.")
    
    ;; Unbuffered mode for I/O ports
    (file-stream-buffer-mode (current-input-port) 'none)
    (file-stream-buffer-mode (current-output-port) 'none)
    (file-stream-buffer-mode (current-error-port) 'none)

    ;; If another instance of the server is running, send credentials for that instance and exit
    (when (file-exists? (build-path (read-config 'seashell) "creds"))
        (logf 'info "Found existing seashell instance lock file; using existing credentials.")
        (with-input-from-file (build-path (read-config 'seashell) "creds") (thunk
          (write (read))))
        (exit 0))
    
    ;; Global dispatcher.
    (define seashell-dispatch
      (sequence:make
        request-logging-dispatcher
        (filter:make #rx"^/$" (make-websocket-dispatcher 
                               (curry conn-dispatch keepalive-chan)))
        (filter:make #rx"^/export/" project-export-dispatcher)
        (filter:make #rx"^/upload$" upload-file-dispatcher)
        standard-error-dispatcher))

    ;; Start the server.
    (define conf-chan  (make-async-channel))
    (define shutdown-server
      (serve
       #:dispatch seashell-dispatch
       #:port 0
       #:tcp@ ssl-unit
       #:listen-ip "localhost"
       #:confirmation-channel conf-chan))
    (define start-result (async-channel-get conf-chan))
    (when (exn? start-result)
      (raise start-result))

    ;; Generate and send credentials, write lock file
    (define host "localhost")
    (logf 'debug (sprintf "Read hostname '~a' from login server" host))
    (define key (seashell-crypt-make-key))
    (install-server-key! key)
    (define creds `#hash((key . ,key) (host . ,host) (port . ,start-result)))
    (write (serialize creds))
    (with-output-to-file (build-path (read-config 'seashell) "creds") (thunk (write (serialize creds))))
    
    ;; Write out the listening port
    (printf "~a~n" start-result)
    (logf 'info "Listening on port ~a." start-result)
    
    ;; Loop and serve requests.
    (with-handlers
        ([exn:break? (lambda(e) (logf 'error "Terminating on break."))])
      (let loop ()
        (match (sync/timeout/enable-break (/ (read-config 'backend-client-idle-timeout) 1000) keepalive-chan)
          [#f (void)]
          [else (loop)])))

    ;; Delete lock file
    (delete-file (build-path (read-config 'seashell) "creds"))
    
    ;; Shutdown.
    (logf 'info "Shutting down...")
    (shutdown-server)
    (logf 'info "Graceful shutdown.")
    (exit 0)))
