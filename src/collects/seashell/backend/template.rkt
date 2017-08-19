#lang typed/racket
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
(require seashell/log
         (submod seashell/seashell-config typed)
         seashell/compiler
         seashell/backend/runner
         typed/net/url
         typed/json)

;; need to do this because typed/net/head contract for extract-field is wrong
(require/typed net/head
  [extract-field (-> (U String Bytes) (U String Bytes) (U String Bytes False))])

(provide call-with-template url-string?)

;; (url-string? str) -> bool?
;; Predicate for testing if a string is a valid URL
(: url-string? (-> String Boolean))
(define (url-string? str)
  (with-handlers
    ([url-exception? (lambda (exn) #f)])
    (define res (string->url str))
    (not (not (url-scheme res)))))

;; (call-with-template source thunk)
;; Calls thunk, passing an input-port referring to the template located at source.
;;
;; Arguments:
;;  source - Path to template, or URL to template.
;;  thunk - Procedure, taking an input-port referring to source.
(: call-with-template (All (A) (-> String (-> Input-Port A) A)))
(define (call-with-template source thunk)
  (cond
    [(url-string? source)
     (define surl (string->url source))
     (cond
       [(string=? (assert (url-scheme surl)) "file")
         (call/input-url (assert surl) get-pure-port thunk)]
       [(string=? (assert (url-scheme surl)) "ssh")
         (match-define (list _ host file) (regexp-match #rx"//(.*@[^:]*):(.*)" source))
         (define-values (sshproc sshout sshin ssherr)
           (subprocess #f #f #f (read-config-string 'ssh-binary)
             "-o" "PasswordAuthentication=no" (assert host) (string-append "cat " (assert file))))
         (close-output-port sshin)
         (dynamic-wind
           void
           (lambda () (let ([err (read-line ssherr)])
                        (if (eof-object? err)
                            (thunk sshout)
                            (raise (exn:fail (format "Error when fetching template: ~a" err)
                                             (current-continuation-marks))))))
           (lambda () (close-input-port sshout)
                      (close-input-port ssherr)))]
       [else
         (define-values (port hdrs) (get-pure-port/headers (assert surl) #:status? #t #:redirections 10))
         (dynamic-wind
           (lambda () #f)
           (lambda ()
             (match-define (list _ status text headers) (regexp-match #rx"^HTTP/1\\.1 ([0-9][0-9][0-9]) ([^\n\r]*)(.*)" hdrs))
             (when (not (equal? status "200"))
               (raise (exn:fail (format "Error when fetching template ~a: ~a ~a." source status text)
                                (current-continuation-marks))))
             (when (not (equal? (string-trim (cast (extract-field "Content-Type" (assert headers)) String))
                                "application/zip"))
               (raise (exn:fail (format "Error when fetching template ~a: template was not a ZIP file." source)
                                (current-continuation-marks))))
             (thunk port))
           (lambda () (close-input-port port)))])]
    [else
      (call-with-input-file source thunk)]))
