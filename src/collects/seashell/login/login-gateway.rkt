#lang racket
;; Seashell's login gateway.
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
(require net/cgi
         net/url
         net/cookie
         net/uri-codec
         seashell/seashell-config
         seashell/support-native
         seashell/log
         seashell/tunnel
         seashell/crypto
         racket/sandbox
         racket/serialize
         json
         xml)

(provide gateway-main)
(define shutdown-custodian (make-custodian))

;; response/json jsexpr -> void
;; Sends CGI output as a JSON expression.
(define (response/json jsexpr)
  (printf "Access-Control-Allow-Origin: *\r\n")
  (printf "Content-Type: application/json\r\n\r\n")
  (write-json jsexpr)
  (printf "\r\n")
  (flush-output))

;; report-error/json code desc -> void
;; Runs response/json to send the error message, and then quits.
(define (report-error/json code desc)
  (response/json `#hash((error . #hash((code . ,code) (message . ,desc)))))
  (custodian-shutdown-all shutdown-custodian)
  (exit 1))

;; report-error/html message traceback -> void
;; Reports the error as an HTML message.
(define (report-error/html code message [traceback ""])
  (printf "Content-Type: text/html\r\n\r\n")
  (display (xexpr->string
             `(html
                (head
                  (title "500 Internal Server Error"))
                (body
                  (h1 "Internal Server Error")
                  (p ,(format "An internal server error was encountered while prcessing your request: (~a) ~a." code message))
                  ,@(if (and (read-config 'debug) exn)
                      `((hr)
                        (pre ,traceback))
                      '(""))
                  (hr)
                  (address ,(format "Seashell-Login/~a running on Racket ~a" SEASHELL_VERSION (version)))))))
  (custodian-shutdown-all shutdown-custodian)
  (exit 1))

;; report-error
;; Wraps report-error/X
(define (report-error code message [traceback ""])
  (if (equal? (get-cgi-method) "POST")
    (report-error/json code message)
    (report-error/html code message traceback)))


;; password-based-login/ajax
;; AJAX-based password login.
;;
;; Required bindings:
;;  u - Username
;;  p - Password
(define (password-based-login/ajax)
  ;; Set up the standard logger for password-based-login/ajax
  ;; (Logs to stderr).
  (standard-logger-setup)
  (define bdgs (get-bindings))

  (define uname
    (let ((l (extract-bindings 'u bdgs)))
      (unless (= (length l) 1)
        (report-error 3 "Bad username provided."))
      (first l)))

  (define passwd
    (let ((l (extract-bindings 'p bdgs)))
      (unless (= (length l) 1)
        (report-error 3 "Bad password provided."))
      (first l)))

  ;; Timeout the login process.
  (with-handlers
    ([exn:fail:resource? (lambda(e)
                           (report-error 7 "Login timed out."))]
     [exn:tunnel?
       (match-lambda
         [(exn:tunnel message marks 7)
          (report-error 5 "Invalid credentials.")]
         [(exn:tunnel message marks 6)
          (report-error 6 "Invalid host key. See server log.")]
         [(exn:tunnel message marks code)
          (report-error 4 (format "Session could not be started (internal error, code=~a)." code))])])
    (with-limits (read-config 'backend-login-timeout) #f
      ;; Terminate existing Seashell instance
      (when (and (not (empty? (extract-bindings "reset" bdgs))) (equal? "true" (extract-binding/single "reset" bdgs)))
        (define creds-tun (password:tunnel-launch uname passwd #:args "-d"))
        (define creds (deserialize (read (tunnel-in creds-tun))))
        (when (eof-object? creds)
          (report-error 4 "Could not reset existing Seashell instance."))
        ;; Get the host and PID
        (define creds-host (hash-ref creds 'host))
        (define creds-pid (hash-ref creds 'pid))
        ;; Kill the process
        (define kill-tun (password:tunnel-launch uname passwd #:target "kill" #:args (format "~a" creds-pid) #:host creds-host))
        (subprocess-wait (tunnel-process kill-tun))
        (when (not (= 0 (subprocess-status (tunnel-process kill-tun))))
          (report-error 4 (format "Could not kill existing Seashell instance (internal error, code=~a)."
                                  (subprocess-status (tunnel-process kill-tun))))))

      ;; Spawn backend process on backend host.
      (define tun (password:tunnel-launch uname passwd))
      (logf 'debug "Tunnel hostname is ~a" (tunnel-hostname tun))

      ;; Send hostname to backend process
      (write (tunnel-hostname tun) (tunnel-out tun))
      (flush-output (tunnel-out tun))

      (logf 'debug "Waiting for tunnel credentials.")

      ;; Get initialization info from backend process
      (define be-creds (read (tunnel-in tun)))

      (when (eof-object? be-creds)
        (report-error 4 (format "Session could not be started; tunnel unexpectedly died!")))

      (logf 'debug "Waiting for tunnel shutdown.")
      ;; Wait for tunnel shutdown.
      (subprocess-wait (tunnel-process tun))

      ;; Check for graceful exit.
      (when (not (= 0 (subprocess-status (tunnel-process tun))))
        (report-error 4 (format "Session could not be started (internal error, code=~a)."
                                (subprocess-status (tunnel-process tun)))))
      
      ;; Send key, address, and port to client.
      ;; This duplicates some code in seashell/crypto.
      (response/json (deserialize be-creds)))))


;; uw-login/redirect
;; UW-based login system + redirect.
(define (uw-login/redirect)
  ;; Set up the standard logger for uw-login/redirect
  ;; Write to the user's log file. 
  (current-error-port (open-output-file (build-path (find-system-path 'home-dir) ".seashell-cgi.log") #:exists 'append))
  (file-stream-buffer-mode (current-error-port) 'none)
  (standard-logger-setup)
  (define bdgs (get-bindings))

  ;; Binding for tunnel process outside scope of with-limits.
  (define tun-proc #f)
  (define tun #f)

  ;; Check the user id
  (unless (= 0 (seashell_uw_check_remote_user))
    (report-error 1 "Invalid credentials!"))

  ;; Timeout the login process.
  (with-handlers
    ([exn:fail:resource? (lambda(e)
                           (report-error 7 "Login timed out."))])
    (with-limits (read-config 'backend-login-timeout) #f
    ;; Terminate existing Seashell instance
    (when (and (not (empty? (extract-bindings "reset" bdgs))) (equal? "true" (extract-binding/single "reset" bdgs)))
      (with-handlers ([exn:fail:filesystem? (lambda (x) #f)])
        (define creds (call-with-input-file (build-path (read-config 'seashell) (read-config 'seashell-creds-name))
                                            (compose deserialize read)))
        (define creds-host (hash-ref creds 'host))
        (define creds-pid (hash-ref creds 'pid))
        (uw:tunnel-launch #:target "kill" #:args (format "~a" creds-pid))))

      ;; Spawn backend process on backend host.
      (set! tun (uw:tunnel-launch))
      (set! tun-proc (tunnel-process tun))

      (logf 'debug "Tunnel hostname is ~a" (tunnel-hostname tun))

      ;; Send hostname to backend process
      (write (tunnel-hostname tun) (tunnel-out tun))
      (flush-output (tunnel-out tun))

      (logf 'debug "Waiting for tunnel credentials.")

      ;; Get initialization info from backend process
      (define be-creds (read (tunnel-in tun)))

      (when (eof-object? be-creds)
        (report-error 4 (format "Session could not be started; tunnel unexpectedly died!")))

      (logf 'debug "Waiting for tunnel shutdown.")
      ;; Wait for tunnel shutdown.
      (subprocess-wait (tunnel-process tun))

      ;; Check for graceful exit.
      (when (not (= 0 (subprocess-status (tunnel-process tun))))
        (report-error 4 (format "Session could not be started (internal error, code=~a)."
                                (subprocess-status (tunnel-process tun)))))

      ;; Credential handling + cookie.
      (define creds (deserialize be-creds))
      (define credentials-cookie
        (cookie:secure
          (cookie:add-path
            (set-cookie (uri-encode (read-config 'seashell-creds-cook)) (uri-encode (jsexpr->string creds)))
            "/~cs136/seashell")
          #t))

      ;; Write response back.
      (printf "Status: 303 See Other\r\n")
      (printf "Set-Cookie: ~a\r\n" (print-cookie credentials-cookie))
      (printf "Location: /~~cs136/seashell/frontend.html\r\n\r\n"))))

;; gateway-main
;; Main login function.
(define (gateway-main)
  ;; Check that HTTPS was set.
  (unless
    (equal? (getenv "HTTPS") "on")
    (report-error 1 "Requires SSL."))
  
  (with-handlers
    ([exn:fail? (lambda (exn) (report-error 1 (exn-message exn) (format-stack-trace
                                                                       (exn-continuation-marks exn))))])
    ;; Install configuration.
    (config-refresh!)

    (parameterize ([current-custodian shutdown-custodian]
                   [current-subprocess-custodian-mode 'interrupt])
      ;; Check which mode we're running as.
      (if (equal? (get-cgi-method) "POST")
        (password-based-login/ajax)
        (uw-login/redirect)))

    ;; Log the successful login attempt.
    (when (read-config 'login-tracking-helper)
      (system* (read-config 'login-tracking-helper))))

  (custodian-shutdown-all shutdown-custodian)
  (exit 0))

