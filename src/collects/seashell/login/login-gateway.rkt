#lang racket
;; Seashell's login gateway.
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
                  (address ,(format "Seashell-Login/1.0 running on Racket ~a" (version)))))))
  (exit 1))

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
        (report-error/json 3 "Bad username provided."))
      (first l)))

  (define passwd
    (let ((l (extract-bindings 'p bdgs)))
      (unless (= (length l) 1)
        (report-error/json 3 "Bad password provided."))
      (first l)))

  ;; Binding for tunnel process outside scope of with-limits.
  (define tun-proc #f)
  (define tun #f)

  ;; Timeout the login process.
  (with-handlers
    ([exn:fail:resource? (lambda(e)
                           (when tun-proc
                             (subprocess-kill tun-proc #t))
                           (when tun
                             (tunnel-close tun))
                           (report-error/json 7 "Login timed out."))])
    (with-limits (read-config 'backend-login-timeout) #f
      ;; Spawn backend process on backend host.
      (set! tun
        (with-handlers
          ([exn:tunnel?
             (match-lambda
               [(exn:tunnel message marks 7)
                (report-error/json 5 "Invalid credentials.")]
               [(exn:tunnel message marks 6)
                (report-error/json 6 "Invalid host key. See server log.")]
               [(exn:tunnel message marks code)
                (report-error/json 4 (format "Session could not be started (internal error, code=~a)." code))])])
          (password:tunnel-launch uname passwd)))

      (set! tun-proc (tunnel-process tun))

      (logf 'debug "Tunnel hostname is ~a" (tunnel-hostname tun))

      ;; Send hostname to backend process
      (write (tunnel-hostname tun) (tunnel-out tun))
      (flush-output (tunnel-out tun))

      (logf 'debug "Waiting for tunnel credentials.")

      ;; Get initialization info from backend process
      (define be-creds (read (tunnel-in tun)))

      (when (eof-object? be-creds)
        (report-error/json 4 (format "Session could not be started; tunnel unexpectedly died!")))

      (logf 'debug "Waiting for tunnel shutdown.")
      ;; Wait for tunnel shutdown.
      (subprocess-wait (tunnel-process tun))

      ;; Check for graceful exit.
      (when (not (= 0 (subprocess-status (tunnel-process tun))))
        (report-error/json 4 (format "Session could not be started (internal error, code=~a)."
                                (subprocess-status (tunnel-process tun)))))

      ;; Close the tunnel
      (tunnel-close tun)

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
    (report-error/html 1 "Invalid credentials!"))

  ;; Timeout the login process.
  (with-handlers
    ([exn:fail:resource? (lambda(e)
                           (when tun-proc
                             (subprocess-kill tun-proc #t))
                           (when tun
                             (tunnel-close tun))
                           (report-error/html 7 "Login timed out."))])
    (with-limits (read-config 'backend-login-timeout) #f
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
        (report-error/html 4 (format "Session could not be started; tunnel unexpectedly died!")))

      (logf 'debug "Waiting for tunnel shutdown.")
      ;; Wait for tunnel shutdown.
      (subprocess-wait (tunnel-process tun))

      ;; Check for graceful exit.
      (when (not (= 0 (subprocess-status (tunnel-process tun))))
        (report-error/html 4 (format "Session could not be started (internal error, code=~a)."
                                (subprocess-status (tunnel-process tun)))))

      ;; Close the tunnel
      (tunnel-close tun)

      ;; Credential handling + cookie.
      (define creds (deserialize be-creds))
      (define credentials-cookie
        (cookie:secure
          (cookie:add-path
            (set-cookie (uri-encode "seashell-session") (uri-encode (jsexpr->string creds)))
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
    (report-error/html 1 "Requires SSL."))
  
  (with-handlers
    ([exn:fail? (lambda (exn) (report-error/html 1 (exn-message exn) (format-stack-trace
                                                                       (exn-continuation-marks exn))))])
    ;; Install configuration.
    (config-refresh!)
    ;; Check which mode we're running as.
    (if (equal? (get-cgi-method) "POST")
      (password-based-login/ajax)
      (uw-login/redirect)))
  (exit 0))

