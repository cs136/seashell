#lang racket/base
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
(provide gateway-main)
(module typed typed/racket
  (require typed/net/cgi
           typed/net/cookie
           typed/net/uri-codec
           typed/racket/sandbox
           (submod seashell/seashell-config typed)
           seashell/support-native
           seashell/log
           seashell/tunnel
           typed/json)

  (require/typed racket/serialize
                 [deserialize (-> Any Any)])
  (require/typed xml
                 [xexpr->string (-> Any String)])

  (provide gateway-main/typed)
  (define shutdown-custodian (make-custodian))

  ;; response/json jsexpr -> void
  ;; Sends CGI output as a JSON expression.
  (: response/json (-> JSExpr Void))
  (define (response/json jsexpr)
    (printf "Access-Control-Allow-Origin: *\r\n")
    (printf "Content-Type: application/json\r\n\r\n")
    (write-json jsexpr)
    (printf "\r\n")
    (flush-output)
    (void))

  ;; report-error/json code desc -> void
  ;; Runs response/json to send the error message, and then quits.
  (: report-error/json (-> Integer String Nothing))
  (define (report-error/json code desc)
    (printf "Status: 500 Internal Server Error\r\n")
    (define result (ann
                     (make-immutable-hash
                      (list (cons 'error
                                   (ann
                                     (make-immutable-hash
                                       (list
                                        (ann (cons 'code code) (Pairof Symbol JSExpr))
                                        (ann (cons 'message desc) (Pairof Symbol JSExpr))))
                                     JSExpr))))
                     JSExpr))
    (response/json result)
    (custodian-shutdown-all shutdown-custodian)
    (exit 1))

  ;; report-error/html message traceback -> void
  ;; Reports the error as an HTML message.
  (: report-error/html (->* (Integer String) (String) Nothing))
  (define (report-error/html code message [traceback ""])
    (printf "Status: 500 Internal Server Error\r\n")
    (printf "Content-Type: text/html\r\n\r\n")
    (display (xexpr->string
               `(html
                  (head
                    (title "500 Internal Server Error"))
                  (body
                    (h1 "Internal Server Error")
                    (p ,(format "An internal server error was encountered while prcessing your request: (~a) ~a." code message))
                    ,@(if (and (read-config-boolean 'debug) exn)
                        `((hr)
                          (pre ,traceback))
                        '(""))
                    (hr)
                    (address ,(format "Seashell-Login/~a running on Racket ~a" SEASHELL_VERSION (version)))))))
    (custodian-shutdown-all shutdown-custodian)
    (exit 1))

  ;; report-error
  ;; Wraps report-error/X
  (: report-error (->* (Integer String) (String) Nothing))
  (: report-error-nocapture (->* (Integer String) (String) Nothing))
  (define (report-error-nocapture code message [traceback ""])
    (if (equal? (get-cgi-method) "POST")
      (report-error/json code message)
      (report-error/html code message traceback)))
  (define (report-error code message [traceback ""])
    (capture-exception (exn:fail (format "seashell-login: ~a: ~a~n~a" code message traceback) (current-continuation-marks)))
    (report-error-nocapture code message traceback))

  ;; report-exception
  ;; Reports an exception
  (: report-exception (-> Integer exn Any))
  (define (report-exception code exn)
    (capture-exception exn)
    (report-error-nocapture code (exn-message exn)
                            (format-stack-trace
                              (exn-continuation-marks exn))))

  ;; password-based-login/ajax
  ;; AJAX-based password login.
  ;;
  ;; Required bindings:
  ;;  u - Username
  ;;  p - Password
  (: password-based-login/ajax (-> Any))
  (define (password-based-login/ajax)
    ;; Set up the standard logger for uw-login/redirect
    ;; Write to the Seashell-CGI log file location.
    (define login-logfile (read-config-optional-path 'seashell-login-logfile))
    (when login-logfile
      (current-error-port (open-output-file login-logfile #:exists 'append))
      (file-stream-buffer-mode (current-error-port) 'none))
    (standard-logger-setup)
    (define bdgs (get-bindings))

    (define uname
      (let ((l (extract-bindings 'u bdgs)))
        (if (not (= (length l) 1))
          (report-error 3 "Bad username provided.")
          (first l))))

    (define passwd
      (let ((l (extract-bindings 'p bdgs)))
        (if (not (= (length l) 1))
          (report-error 3 "Bad password provided.")
          (first l))))

    ;; Timeout the login process.
    (with-handlers
      ([exn:fail:resource? (lambda (_)
                             (report-error 7 "Login timed out."))]
       [exn:tunnel?
         (match-lambda
           [(exn:tunnel message marks 7)
            (report-error 5 "Invalid credentials.")]
           [(exn:tunnel message marks 6)
            (report-error 6 "Invalid host key. See server log.")]
           [(exn:tunnel message marks code)
            (report-error 4 (format "Session could not be started (internal error, code=~a)." code))])])
      (call-with-limits (read-config-integer 'backend-login-timeout) #f
        (lambda ()
          ;; Terminate existing Seashell instance
          (when (and (not (empty? (extract-bindings "reset" bdgs))) (equal? "true" (extract-binding/single "reset" bdgs)))
            (define creds-tun (password:tunnel-launch uname passwd #:target (read-config-string 'seashell-main) #:args "-d"))
            (define raw-creds (read (tunnel-in creds-tun)))
            (when (eof-object? raw-creds)
              (report-error 4 "Could not reset existing Seashell instance."))
            (when raw-creds
              (define creds (cast (deserialize raw-creds) (HashTable Symbol Any)))
              ;; Get the host and PID
              (define creds-host (cast (hash-ref creds 'host) String))
              (define creds-pid (cast (hash-ref creds 'pid) Integer))
              ;; Kill the process
              (define kill-tun (password:tunnel-launch uname passwd #:target "kill" #:args (format "~a" creds-pid) #:host creds-host))
              (subprocess-wait (tunnel-process kill-tun))
              (when (not (equal? 0 (subprocess-status (tunnel-process kill-tun))))
                (report-error 4 (format "Could not kill existing Seashell instance (internal error, code=~a)."
                                        (subprocess-status (tunnel-process kill-tun)))))))

          ;; Spawn backend process on backend host.
          (define tun (password:tunnel-launch uname passwd))
          (logf 'debug "Tunnel hostname is ~a" (tunnel-hostname tun))

          ;; Send hostname to backend process
          (write (tunnel-hostname tun) (tunnel-out tun))
          (flush-output (tunnel-out tun))

          (logf 'debug "Waiting for tunnel credentials.")

          ;; Get initialization info from backend process
          (define be-creds (read (tunnel-in tun)))

          (cond
            [(eof-object? be-creds)
             (report-error 4 (format "Session could not be started; tunnel unexpectedly died!"))]
            [else
             (logf 'debug "Waiting for tunnel shutdown.")
             ;; Wait for tunnel shutdown.
             (subprocess-wait (tunnel-process tun))

             ;; Check for graceful exit.
             (when (not (equal? 0 (subprocess-status (tunnel-process tun))))
               (report-error 4 (format "Session could not be started (internal error, code=~a)."
                                       (subprocess-status (tunnel-process tun)))))

             ;; Send key, address, and port to client.
             ;; This duplicates some code in seashell/crypto.
             (response/json (cast (deserialize be-creds) JSExpr))])))))


  ;; uw-login/redirect
  ;; UW-based login system + redirect.
  (define (uw-login/redirect)
    ;; Set up the standard logger for uw-login/redirect
    ;; Write to the user's log file.
    (current-error-port (open-output-file (build-path (find-system-path 'home-dir) ".seashell-cgi.log") #:exists 'append))
    (file-stream-buffer-mode (current-error-port) 'none)
    (standard-logger-setup)
    (define bdgs (get-bindings))

    ;; Check the user id
    (unless (= 0 (seashell_uw_check_remote_user))
      (report-error 1 "Invalid credentials!"))

    ;; Timeout the login process.
    (with-handlers
      ([exn:fail:resource? (lambda(e)
                             (report-error 7 "Login timed out."))])
      (call-with-limits (read-config-integer 'backend-login-timeout) #f
       (lambda ()
        ;; Terminate existing Seashell instance
        (when (and (not (empty? (extract-bindings "reset" bdgs))) (equal? "true" (extract-binding/single "reset" bdgs)))
          (with-handlers ([exn:fail:filesystem? (lambda (x) #f)])
            (define creds (cast (call-with-input-file (build-path (read-config-path 'seashell) (read-config-string 'seashell-creds-name))
                                                      (compose deserialize read))
                                (HashTable Symbol Any)))
            (define creds-host (cast (hash-ref creds 'host) String))
            (define creds-pid (cast (hash-ref creds 'pid) Integer))
            (uw:tunnel-launch #:target "kill" #:args (format "~a" creds-pid))))

          ;; Spawn backend process on backend host.
          (define tun (uw:tunnel-launch))
          (define tun-proc (tunnel-process tun))

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
          (when (not (equal? 0 (subprocess-status (tunnel-process tun))))
            (report-error 4 (format "Session could not be started (internal error, code=~a)."
                                    (subprocess-status (tunnel-process tun)))))

          ;; Credential handling + cookie.
          (define creds (cast (deserialize be-creds) JSExpr))
          (define credentials-cookie
            (cookie:secure
              (cookie:add-path
                (set-cookie (uri-encode (read-config-string 'seashell-creds-cook)) (uri-encode (jsexpr->string creds)))
                "/~cs136/seashell")
              #t))

          ;; Write response back.
          (printf "Status: 303 See Other\r\n")
          (printf "Set-Cookie: ~a\r\n" (print-cookie credentials-cookie))
          (printf "Location: /~~cs136/seashell/frontend.html\r\n\r\n")))))

  ;; gateway-main
  ;; Main login function.
  (define (gateway-main/typed)
    ;; Check that HTTPS was set.
    (unless
      (equal? (getenv "HTTPS") "on")
      (report-error 1 "Requires SSL."))

    (with-handlers
      ([exn:fail? (lambda ([exn : exn]) (report-exception 1 exn))])
      ;; Install configuration.
      (config-refresh!)

      (parameterize ([current-custodian shutdown-custodian]
                     [current-subprocess-custodian-mode 'interrupt])
        ;; Check which mode we're running as.
        (if (equal? (get-cgi-method) "POST")
          (password-based-login/ajax)
          (uw-login/redirect)))

      ;; Log the successful login attempt.
      (define tracking (read-config-optional-path 'login-tracking-helper))
      (when tracking (system* tracking)))

    (custodian-shutdown-all shutdown-custodian)
    (exit 0)))

(require (submod "." typed))
(define (gateway-main)
  (gateway-main/typed))
