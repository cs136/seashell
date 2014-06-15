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
         racket/udp
         seashell/websocket
         seashell/log
         seashell/seashell-config
         seashell/support-native
         seashell/overrides/ssl-unit-tcp
         seashell/backend/dispatch
         seashell/backend/project
         seashell/backend/http-dispatchers
         seashell/backend/authenticate
         seashell/compiler
         seashell/crypto
         web-server/web-server
         web-server/http/xexpr
         web-server/http/request-structs
         web-server/dispatchers/dispatch
         ffi/unsafe/atomic
         (prefix-in sequence: web-server/dispatchers/dispatch-sequencer)
         (prefix-in filter: web-server/dispatchers/dispatch-filter))

(provide backend-main)

;; creds-valid? creds credentials-file -> boolean?
;; Checks whether or not the given creds point to a running seashell instance
;; TODO: verify that response came from seashell
(define/contract (creds-valid? creds credentials-file)
  (-> hash? path? boolean?)
  (cond
    [(hash-has-key? creds 'ping-port)
     ;; Ensure a consistent family.
     (define sock (udp-open-socket (hash-ref creds 'host)
                                   (hash-ref creds 'port)))
     (udp-send-to sock (hash-ref creds 'host) (hash-ref creds 'ping-port) #"ping")
     (define success (sync/timeout (read-config 'seashell-ping-timeout)
                                   (udp-receive!-evt sock (make-bytes 0))))
     (udp-close sock)

     ;; Remove stale credentials files.  This can be a race condition,
     ;; so make sure the file is locked with fcntl before continuing.
     (unless success
       (logf 'info "Credentials file was stale; regenerating.")
       (delete-file credentials-file))
     (if success #t #f)]
    [else #f]))

;; Channel used to keep process alive.
(define keepalive-chan (make-async-channel))

;; init-environment -> void?
;; Sets up the Seashell project environment
(define/contract (init-environment)
  (-> void?)
  (when (not (directory-exists? (read-config 'seashell)))
    (make-directory (read-config 'seashell))))

;; exit-from-seashell return -> any/c
;; Seashell-specific exit function.
;;
;; Currently just exits.
(define (exit-from-seashell return)
  (exit return))

;; detach any/c
;; Detaches backend.
(define (detach)
  (call-as-atomic
    (thunk
      (flush-output (current-output-port))
      (unless (= 0 (seashell_signal_detach))
        (exit-from-seashell 5)))))

;; (backend/main)
;; Entry point to the backend server.
;;
;; This function is invoked directly from login-process.c
(define (backend-main)
  (define ssl-unit #f)

  (with-handlers
      ([exn:fail? 
        (lambda (exn)
          (fprintf (current-error-port) "Exception raised in startup code: ~a~n" (exn-message exn))
          (exit-from-seashell 2))])
    ;; Load configuration.
    (config-refresh!)
    
    ;; Install SSL keys.
    (set! ssl-unit
      (make-ssl-tcp@
       (read-config 'ssl-cert)
       (read-config 'ssl-key)
       #f #f #f #f #f))
    
    ;; Dropping permissions.
    (unless (= 0 (seashell_drop_permissions))
      (fprintf (current-error-port) "Failed to drop permissions!  Exiting...~n")
      (exit-from-seashell 1))
    
    ;; Directory setup.
    (init-environment)
    (init-projects)
    
    ;; Replace stderr with a new port that writes to a log file in the user's Seashell directory.
    (current-error-port (open-output-file (build-path (read-config 'seashell) "seashell.log")
                                          #:exists 'append))
    ;; Note: tunnel.c does not loop until EOF on stderr, so it's OK to leave the old file descriptor
    ;;  open.
    
    ;; Unbuffered mode for output ports.
    (file-stream-buffer-mode (current-output-port) 'none)
    (file-stream-buffer-mode (current-error-port) 'none)

    ;; Install umask.
    ;; NOTE: This will not affect the permissions set for the ~/.seashell/{projects,runtime-files} directories.
    (seashell_set_umask)
    
    ;; Logging setup.
    (standard-logger-setup))
  
  (with-handlers
      ([exn:fail? 
        (lambda (exn)
          (logf 'error "Exception raised in Seashell: ~a~n" (exn-message exn))
          (exit-from-seashell 3))])
    
    (logf 'info "Starting up.")
    
    ;; If another instance of the server is running, send credentials for that instance and exit-from-seashell
    (define credentials-file (build-path (read-config 'seashell) "creds"))
    (define credentials-port #f)
    (define shutdown-server void)
    
    ;; Note: (exit-from-seashell ...) does not unwind the continuation stack.
    (dynamic-wind
     void
     (thunk
      (call-with-continuation-barrier
       (thunk
        ;; Install credentials or quit.
        (set! credentials-port
              (let loop ([tries 0])
                (when (> tries 5)
                  (logf 'info "Error opening credentials file - aborting!")
                  (exit-from-seashell 4))
                (define repeat (make-continuation-prompt-tag))
                (call-with-continuation-prompt
                 (thunk
                  ;; Try to read the file...
                  (with-handlers
                      ([exn:fail:filesystem? (lambda (exn) (void))])
                    (with-input-from-file
                        credentials-file
                      (thunk
                        (with-handlers
                          ([exn:fail:read? (lambda (exn)
                                             (sleep 1)
                                             (abort-current-continuation repeat))])
                         (define result (read))
                         (if (or (eof-object? result)
                                 (not (try-and-lock-file (current-input-port)))
                                 (not (creds-valid? (deserialize result) credentials-file)))
                             (begin
                               (sleep 1)
                               (abort-current-continuation repeat))
                             (write result)))))
                    (logf 'info "Found existing Seashell instance; using existing credentials.")
                    (exit-from-seashell 0))
                  ;; If it does not exist, create it mode 600 and get a port to it.
                  ;; This can lead to a race condition with the above code, so if we can't create it,
                  ;; we loop again (up to a certain number of times)
                  (when (not (= 0 (seashell_create_secret_file credentials-file)))
                    (abort-current-continuation repeat))
                  (with-handlers*
                   ([exn:fail:filesystem? (lambda (exn) (abort-current-continuation repeat))])
                   (open-output-file credentials-file #:exists 'must-truncate)))
                 repeat
                 (thunk (loop (add1 tries))))))
        ;; Set permissions on the file.
        (file-or-directory-permissions
         credentials-file
         user-read-bit)
        
        ;; Global dispatcher.
        (define seashell-dispatch
          (sequence:make
           request-logging-dispatcher
           (filter:make #rx"^/$" (make-websocket-dispatcher 
                                  (curry conn-dispatch keepalive-chan)))
           (filter:make #rx"^/export/" project-export-dispatcher)
           (filter:make #rx"^/upload$" upload-file-dispatcher)
           standard-error-dispatcher))

        ;; Start our places.
        (seashell-compile-place/init)
        
        ;; Start the server.
        (define conf-chan  (make-async-channel))
        (set! shutdown-server
              (serve
               #:dispatch seashell-dispatch
               #:port 0
               #:tcp@ ssl-unit
               #:listen-ip #f
               #:confirmation-channel conf-chan))
        (define start-result (async-channel-get conf-chan))
        (when (exn? start-result)
          (raise start-result))

        ;; Get current username
        (define username (or (seashell_get_username) "unknown_user"))
        
        ;; Start the UDP ping listener
        (define sock (udp-open-socket))
        (udp-bind! sock #f 0)
        (define-values (_1 ping-port _2 _3) (udp-addresses sock #t))
        (define (udp-listen-ping)
          (define-values (_ client-host client-port) (udp-receive! sock (make-bytes 0)))
          (udp-send-to sock client-host client-port #"pong")
          (udp-listen-ping))
        (thread udp-listen-ping)
        
        ;; Generate and send credentials, write lock file
        (define host (read))
        (logf 'debug "Read hostname '~a' from login server." host)
        (define key (seashell-crypt-make-key))
        (install-server-key! key)
        (define creds 
          `#hash((key . ,(seashell-crypt-key->client key))
                 (host . ,host)
                 (port . ,start-result)
                 (ping-port . ,ping-port)
                 (user . ,username)))
        
        ;; Write credentials back to file and to tunnel.
        (write (serialize creds))
        (write (serialize creds) credentials-port)
        
        ;; Detach from backend, and close the credentials port.
        (close-output-port credentials-port)
        (detach)
        
        ;; Write out the listening port
        (logf 'info "Listening on port ~a." start-result)
        
        ;; Loop and serve requests.
        (with-handlers
            ([exn:break? (lambda(e) (logf 'info "Terminating on break."))])
          (let loop ()
            (match (sync/timeout/enable-break (/ (read-config 'backend-client-idle-timeout) 1000) keepalive-chan)
              [#f (void)]
              [else (loop)]))))))
     (thunk
      (logf 'info "Shutting down...")
      ;; Close port (if not closed)
      (unless (or (not credentials-port)
                  (port-closed? credentials-port))
        (close-output-port credentials-port))
      ;; Shutdown server
      (shutdown-server)
      ;; Delete lock file
      (delete-file credentials-file)))
    
    (logf 'info "Graceful shutdown.") 
    (exit-from-seashell 0)))
