#lang racket/base
;; Seashell's backend server.
;; Copyright (C) 2013-2015 The Seashell Maintainers.
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
(require racket/contract
         racket/async-channel
         racket/serialize
         racket/udp
         racket/sandbox
         racket/match
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
         web-server/http/request-structs
         ffi/unsafe/atomic
         mzlib/os
         (prefix-in sequence: web-server/dispatchers/dispatch-sequencer)
         (prefix-in filter: web-server/dispatchers/dispatch-filter))

(provide backend-main dump-creds)

;; exn:creds -- Exception denoting an issue processing Seashell credentials.
(struct exn:creds exn:fail:user ())

;; (_dump-creds)/(dump-creds) -> creds
;; Dumps the current credentials.  Purges old/invalid/stale credentials.
;;
;; dump-creds is provided as an entry point from seashell-main.  Therefore,
;; it sets up the logging system in addition to dumping the credentials.
;;
;; Internal users will probably want to use _dump-creds instead of
;; dump-creds.
;;
;; Raises one of:
;;  exn:fail:filesystem -- file does not exist.
;;  exn:creds           -- credentials does not exist or stale.
(define/contract (_dump-creds)
  (-> list?)
  (define credentials-file (build-path (read-config 'seashell) (read-config 'seashell-creds-name)))
  ;; creds-valid? creds credentials-file -> boolean?
  ;; Checks whether or not the given creds point to a running seashell instance
  (define/contract (creds-valid? creds)
    (-> hash? boolean?)
    (match creds
      [(hash-table ('host ping-host) ('ping-port ping-port) ('port port) (_ _) ...)
       (define success
         (let ([sock (udp-open-socket ping-host ping-port)])
           (dynamic-wind
            (lambda () (void))
            (lambda ()
             ;; This is slightly less reliable than just using udp-connect!
             ;; but it avoids issues when the UDP server responds with a different
             ;; IP address...
             ;;
             ;; We send a 32-bit integer, and we expect the result to be that 32-bit integer XOR the target's TCP port 
             (define ticket (random 4294967087))
             (define challenge (integer->integer-bytes ticket 4 #f))
             (define response (bitwise-xor ticket port))
             (define expected  (integer->integer-bytes response 4 #f))
             (define result (make-bytes 4))
             (udp-send-to sock ping-host ping-port challenge)
             (logf 'debug "Sent ping to ~a:~a! (~a ^ ~a -> ~a)::~v" ping-host ping-port ticket port response expected)
             (define response? (sync/timeout (read-config 'seashell-ping-timeout)
                                             (udp-receive!-evt sock result)))
             (logf 'debug "Got ping back: ~a/~v" response? result)
             (and response? (equal? result expected)))
            (lambda () (udp-close sock)))))
       ;; Remove stale credentials files.  This can be a race condition,
       ;; so make sure the file is locked with fcntl before continuing.
       ;; XXX you are not locking the file here...
       (unless success
         (logf 'info "Credentials file was stale; regenerating.")
         (delete-file credentials-file))
       (and success #t)]
      [else
       ;; Outdated credentials file format. Delete it.
       (logf 'info "Purging old credentials file...")
       (delete-file credentials-file)
       #f]))

  (with-handlers
    ([exn:creds? (lambda (exn)
                   (logf 'warning "Invalid credentials file detected, purging...")
                   (delete-file credentials-file)
                   (raise exn))])
    (call-with-output-file
        credentials-file
      (lambda (lock-file)
        (with-input-from-file
            credentials-file
          (lambda ()
           (with-handlers
               ([exn:fail:read? (lambda (exn)
                                  (raise (exn:creds "Could not read credentials file!" (current-continuation-marks))))])
             (define result (read))
             (if (or (eof-object? result)
                     (not (try-and-lock-file lock-file))
                     (not (creds-valid? 
                            (with-handlers
                              ([exn:fail? (lambda (exn)
                                            (raise exn:creds "Invalid contents in credentials file!"))])
                              (deserialize result)))))
                 (raise (exn:creds "Could not read credentials file!" (current-continuation-marks)))
                 result)))))
      #:exists 'update)))
(define (dump-creds)
  (standard-logger-setup)
  (_dump-creds))


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
   (lambda ()
    (flush-output (current-output-port))
    (unless (= 0 (seashell_signal_detach))
      (exit-from-seashell 5)))))

;; make-udp-ping-listener our-port -> (values integer? custodian?)
;; Creates the UDP ping listener, and returns a custodian that can shut it down.
(define/contract (make-udp-ping-listener our-port)
  (-> integer? (values integer? custodian?))
  (parameterize ([current-custodian (make-custodian (current-custodian))])
    ;; Start the UDP ping listener
    (define sock (udp-open-socket "::0"))
    (udp-bind! sock #f 0)
    (define-values (_1 ping-port _2 _3) (udp-addresses sock #t))
    (thread
     (lambda ()
      (let loop ()
        (define challenge (make-bytes 4))
        (define-values (_ client-host client-port) (udp-receive! sock challenge))
        (thread
          (lambda ()
            (define ticket (integer-bytes->integer challenge #f))
            (define response (bitwise-xor ticket our-port))
            (define result (integer->integer-bytes response 4 #f)) 
            (logf 'debug "Ping from ~a! (~a ^ ~a -> ~a)::~v" client-host ticket our-port response result)
            (udp-send-to sock client-host client-port result)))
        (loop))))
    (values ping-port (current-custodian))))

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
    (define credentials-file (build-path (read-config 'seashell) (read-config 'seashell-creds-name)))
    (define credentials-port #f)
    (define shutdown-server void)
    (define shutdown-listener void)
    
    ;; Note: (exit-from-seashell ...) does not unwind the continuation stack.
    (dynamic-wind
     void
     (lambda ()
      (call-with-continuation-barrier
       (lambda ()
        (call-with-limits 
         #f (read-config 'server-memory-limit)
         (lambda ()
          ;; Install credentials or quit.
          (set! credentials-port
                (let loop ([tries 0])
                  (when (> tries 5)
                    (logf 'error "Error opening credentials file - aborting!")
                    (exit-from-seashell 4))
                  (define repeat (make-continuation-prompt-tag))
                  (call-with-continuation-prompt
                   (lambda ()
                    ;; Try to read the file...
                    (with-handlers
                        ([exn:fail:filesystem? 
                          (lambda (exn)
                            (cond
                              ;; Case 1: File does not exist.
                              [(not (file-exists? credentials-file))
                               (void)]
                              ;; Case 2: Something else...
                              [else (logf 'warning "Retrying error on credentials file: ~a" (exn-message exn))
                                    (abort-current-continuation repeat)]))]
                         [exn:creds?
                          (lambda (exn) (abort-current-continuation repeat))])
                      (write (_dump-creds))
                      (logf 'info "Found existing Seashell instance; using existing credentials.")
                      (exit-from-seashell 0))
                    (logf 'info "Attempting to create new credentials file...")
                    ;; If it does not exist, create it mode 600 and get a port to it.
                    ;; This can lead to a race condition with the above code, so if we can't create it,
                    ;; we loop again (up to a certain number of times)
                    (when (not (= 0 (seashell_create_secret_file credentials-file)))
                      (abort-current-continuation repeat))
                    (with-handlers*
                     ([exn:fail:filesystem? (lambda (exn) (abort-current-continuation repeat))])
                     (open-output-file credentials-file #:exists 'must-truncate)))
                   repeat
                   (lambda () 
                    (sleep (expt 2 tries))
                    (loop (add1 tries))))))
          
          ;; Global dispatcher.
          (define seashell-dispatch
            (sequence:make
             request-logging-dispatcher
             (filter:make #rx"^/$" (make-websocket-dispatcher 
                                    (lambda (conn state)
                                      (conn-dispatch keepalive-chan conn state))))
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
          
          ;; Start the UDP ping listener.
          (define-values (ping-port udp-custodian)
            (make-udp-ping-listener start-result))
          (set! shutdown-listener
                (lambda () (custodian-shutdown-all udp-custodian)))
          
          ;; Get current username
          (define username (or (seashell_get_username) "unknown_user"))
          
          ;; Generate and send credentials, write lock file
          (define host (read))
          (logf 'debug "Read hostname '~a' from login server." host)
          (define key (seashell-crypt-make-key))
          (install-server-key! key)
          (define creds 
            `#hash((key . ,(seashell-crypt-key->client key))
                   (host . ,host)
                   (port . ,start-result)
                   (pid . ,(getpid))
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
                [else (loop)]))))))))
     (lambda ()
      (logf 'info "Shutting down...")
      ;; Close port (if not closed)
      (unless (or (not credentials-port)
                  (port-closed? credentials-port))
        (close-output-port credentials-port))
      ;; Shutdown listner
      (shutdown-listener)
      ;; Shutdown server
      (shutdown-server)
      ;; Delete lock file - but suppress any errors.
      (with-handlers ([exn:fail:filesystem? (lambda (exn) (void))])
        (delete-file credentials-file))))
    
    (logf 'info "Graceful shutdown.") 
    (exit-from-seashell 0)))
