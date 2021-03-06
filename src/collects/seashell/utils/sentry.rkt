#lang typed/racket
;; Typed Racket Sentry bindings;
;; Copyright (C) 2017 The Seashell Maintainers
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
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
(require typed/json
         typed/net/url
         typed/racket/date
         "typed-json-struct.rkt"
         "uuid.rkt")
(require/typed racket/hash
               [hash-union (->* ((HashTable Symbol JSExpr) (HashTable Symbol JSExpr)) (#:combine (-> Any Any Any)) (HashTable Symbol JSExpr))])
(provide sentry-reporter%)

(struct exn:fail:sentry exn:fail ())

(: check-present (All (A) (-> String (Option A) A)))
(define (check-present name optional)
  (if optional optional
      (raise (exn:fail:sentry (format "Required argument ~a not present!" name)
                              (current-continuation-marks)))))

(define sentry-auth-with-secret
  "X-Sentry-Auth: Sentry sentry_version=7, sentry_client=seashell-sentry/1.0, sentry_timestamp=~a, sentry_key=~a, sentry_secret=~a")
(define sentry-auth-without-secret
  "X-Sentry-Auth: Sentry sentry_version=7, sentry_client=seashell-sentry/1.0, sentry_timestamp=~a, sentry_key=~a")

(json-struct log-entry ([category : String] [message : String] [timestamp : Integer]) #:transparent)

;; sentry-reporter%
;; Class representing a connection to Sentry
;;
;; Construction Arguments:
;;  opt-dsn: Sentry authentication URI
;;  origin: Origin to use (if using public key only)
;;  tags: Tags to tag each message with
;;  log-buffer-size: Length of log buffer (default 100)
(define sentry-reporter%
  (class object%
    (init [opt-dsn : (U False String) #f]
          [tags : (HashTable Symbol JSExpr) (make-immutable-hash)]
          [origin : (U False String) #f]
          [log-buffer-size : Integer 100])

    ;; logs -- Last 100 log entries, in reverse order.
    (: logs (Listof log-entry))
    (define logs '())
    (: _log-buffer-size Integer)
    (define _log-buffer-size log-buffer-size)

    ;; capture-log-entry -- Captures a log entry.
    ;; Arguments:
    ;;  level -- log level.
    ;;  message -- log message.
    ;;  timestamp -- log timestamp.
    (: capture-log-entry (-> Symbol String Integer Any))
    (define/public (capture-log-entry level message timestamp)
      (set! logs (cons (log-entry (symbol->string level) message timestamp) logs))
      ;; Keep only the last 100 log entries
      (when ((length logs) . > . _log-buffer-size)
        (set! logs (take logs _log-buffer-size))))

    (: _dsn (Option String))
    (define _dsn opt-dsn)

    (: dsn URL)
    (define dsn
      (let ([_dsn opt-dsn])
        (cond
          [_dsn (string->url _dsn)]
          [else (string->url "https://disabled/")])))

    (: scheme String)
    (define scheme (if opt-dsn (check-present "URL scheme" (url-scheme dsn)) ""))

    (: keys String)
    (define keys (if opt-dsn (check-present "Sentry keys" (url-user dsn)) ""))

    (: host String)
    (define host (if opt-dsn (check-present "Sentry host" (url-host dsn)) ""))

    (: id String)
    (define id
      (if opt-dsn
        (let
          ([_target (url-path dsn)])
          (if (empty? _target)
              (raise (exn:fail:sentry "Required argument Sentry ID not present!"
                     (current-continuation-marks)))
              (let
                  ([id (path/param-path (last _target))])
                (assert id string?))))
        ""))

    (: target URL)
    (define target (url scheme #f host #f #t
                        `(,(path/param "api" '()) ,(path/param id '()) ,(path/param "store" '()) ,(path/param "" '()))
                        '() #f))
    (super-new)

    (: get-public-key (-> String))
    (define (get-public-key)
      (define result (regexp-match #rx"([^:]+):?([^:]+)?$" keys))
      (assert result)
      (assert (second result) string?))

    (: get-secret-key (-> (Option String)))
    (define (get-secret-key)
      (define result (regexp-match #rx"([^:]+):?([^:]+)?$" keys))
      (assert result)
      (third result))

    (: make-header (-> String))
    (define (make-header)
      (define secret (get-secret-key))
      (if secret
          (format sentry-auth-with-secret (current-seconds) (get-public-key) (get-secret-key))
          (format sentry-auth-without-secret (current-seconds) (get-public-key))))

    (: context (Thread-Cellof JSExpr))
    (define context (make-thread-cell #{(make-immutable-hash) :: JSExpr} #t))

    ;; get/set-{user/email}!
    ;; Gets/sets Sentry user.
    ;; Handled on a per-thread (inherited) basis.
    ;;
    ;; Arguments:
    ;;  user -- Context to set (JSExpr)
    (define user (make-thread-cell "unknown user" #t))
    (define email (make-thread-cell "unknown@no-user.net" #t))
    (: get-user (-> String))
    (define/public (get-user) (thread-cell-ref user))
    (: set-user! (-> String Any))
    (define/public (set-user! _user)
      (thread-cell-set! user _user))
    (: get-email (-> String))
    (define/public (get-email) (thread-cell-ref email))
    (: set-email! (-> String Any))
    (define/public (set-email! _email)
      (thread-cell-set! email _email))

    (: generate-stack-trace (-> exn JSExpr))
    (define (generate-stack-trace exn)
      (define ctx (continuation-mark-set->context (exn-continuation-marks exn)))
      `#hasheq((frames .
         ,(reverse
            (for/list : (Listof (HashTable Symbol JSExpr))
              ([item : (Pairof (U False Symbol) Any) (in-list ctx)])
              (define name (symbol->string (or (car item) '|<unknown procedure>|)))
              (define-values (source line column)
                (let ([loc (cdr item)])
                  (cond
                    [(srcloc? loc)
                     (values (format "~a" (or (srcloc-source loc) "<unknown file>"))
                             (or (srcloc-line loc) 0)
                             (or (srcloc-column loc) 0))]
                    [else (values "<unknown file>" 0 0)])))
              #{`#hasheq((filename . ,source) (function . ,name) (module . ,source)
                         (lineno . ,line) (colno . ,column)) :: (HashTable Symbol JSExpr)})))))

    (: send-packet (-> String String (HashTable Symbol JSExpr) (HashTable Symbol JSExpr) Boolean Any))
    (define (send-packet culprit message packet local-tags block?)
      (define id (string-replace (uuid-generate) "-" ""))
      (define timestamp
        (parameterize ([date-display-format 'iso-8601])
          (date->string (seconds->date (current-seconds) #f) #t)))
      (define all-tags (hash-union tags local-tags #:combine (lambda (t l) l)))
      (define partial-packet
        #{`#hasheq((event_id . ,id)
                   (sentry.interfaces.User .
                    ,#{`#hasheq((id . ,(get-user)) (email . ,(get-email))) :: (HashTable Symbol JSExpr)})
                   (culprit . ,culprit)
                   (timestamp . ,timestamp)
                   (tags . ,(hash-map all-tags (lambda ([k : Symbol] [v : JSExpr]) #{(list (symbol->string k) v) :: JSExpr})))
                   (breadcrumbs . ,((->json (Listof log-entry)) (reverse logs)))
                   (message . ,message)) :: (HashTable Symbol JSExpr)})
      (define full-packet (hash-union partial-packet packet))
      (define header (make-header))
      (define headers `(,header
                        "Content-Type: application/json"
                        ,@(if origin (list (format "Referer: ~a" origin)
                                           (format "Origin: ~a" origin)) '())))
      ;; Block if we have to
      (define report
         (thunk
          (define port (post-impure-port target (jsexpr->bytes full-packet) headers))
          (define result-headers (purify-port port))
          (define result (regexp-match #rx"HTTP/(?:[0-9.]*) ([0-9]*)" result-headers))
          (cond
            [(not result) (raise (exn:fail "Could not read Sentry response!" (current-continuation-marks)))]
            [(not (equal? "200" (second result)))
             (raise (exn:fail (format "Sentry response failed: ~a!" (port->string port))
                              (current-continuation-marks)))])
          (close-input-port port)))
      (if block? (report) (thread report)))

    ;; report-exception
    ;; Reports an exception to Sentry
    ;;
    ;; Arguments:
    ;;  exception - Exception to send
    ;;  local-tags -- Tags to tag message with
    ;;  block -- Block on Sentry I/O?
    (: report-exception (->* (exn (HashTable Symbol JSExpr)) (Boolean) Any))
    (define/public (report-exception exn local-tags [block #f])
      (when _dsn
        (define ctx
          (let ([_ctx (continuation-mark-set->context (exn-continuation-marks exn))])
            (if (empty? _ctx) (list (cons '|<no stack frame available>| #f)) _ctx)))
        (define to-blame (symbol->string (or (car (first ctx)) '|<unknown procedure>|)))
        (define module
          (let ([location (cdr (first ctx))])
            (cond
              [(srcloc? location)
               (format "~a" (srcloc-source location))]
              [else "<unknown source file>"])))
        (define exception-packet
          `#hasheq((exception .
            ,#{`#hasheq((values .
              ,#{`(,#{`#hasheq((type . ,(assert (second (assert (regexp-match #rx"struct:(.*)"
                                         (format "~a" (vector-ref (struct->vector exn) 0)))))))
                       (value . ,(exn-message exn))
                       (module . ,module)
                       (stacktrace . ,(generate-stack-trace exn))) :: JSExpr}) :: JSExpr})) :: JSExpr})))
        (send-packet to-blame (exn-message exn) exception-packet local-tags block)))
))
