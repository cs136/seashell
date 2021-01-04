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
(require seashell/log
         seashell/seashell-config
         seashell/compiler
         seashell/backend/runner
         net/url
         net/head
         json
         file/zip
         file/unzip
         racket/contract
         racket/file
         racket/path
         racket/match
         racket/string
         racket/list
         racket/port)
(provide call-with-template url-string?)

;; (url-string? str) -> bool?
;; Predicate for testing if a string is a valid URL
(define/contract (url-string? str)
  (-> string? boolean?)
  (with-handlers
    ([url-exception? (lambda (exn) #f)])
    (define res (string->url str))
    (not (not (url-scheme res)))))

;; (call-with-template source thunk)
;; Calls thunk, passing an input-port refering to the template located at source.
;;
;; Arguments:
;;  source - Path to template, or URL to template.
;;  thunk - Procedure, taking an input-port referring to source.
(define/contract (call-with-template source thunk)
  (-> (or/c path-string? url-string?) procedure? any/c)
  (cond
    [(url-string? source)
     (define surl (string->url source))
     (cond
       [(string=? (url-scheme surl) "file")
         (call/input-url surl get-pure-port thunk)]
       [(string=? (url-scheme surl) "ssh")
         (match-define (list _ host file) (regexp-match #rx"//(.*@[^:]*):(.*)" source))
         (logf 'info "Doing subprocess ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a"
               (read-config 'ssh-binary) "-q" "-o" "StrictHostKeyChecking=no" "-o" "UserKnownHostsFile=/dev/null"
               "-o" "PasswordAuthentication=no" host (string-append "cat " file))
         (define-values (sshproc sshout sshin ssherr)
           (subprocess #f #f #f (read-config 'ssh-binary)
             "-q" "-o" "StrictHostKeyChecking=no" "-o" "UserKnownHostsFile=/dev/null"
             "-o" "PasswordAuthentication=no" host (string-append "cat " file)))
         (close-output-port sshin)
         (dynamic-wind
           void
           (lambda () (let ([out (port->bytes sshout)]
                            [err (port->bytes ssherr)])
                        (if (equal? (bytes-length err) 0)
                            (thunk (open-input-bytes out))
                            (raise (exn:fail (format "Error when fetching template: ~a" err))))))
           (lambda () (close-input-port sshout)
                      (close-input-port ssherr)))]
       [else
         (define-values (port hdrs) (get-pure-port/headers surl #:status? #t #:redirections 10))
         (dynamic-wind
           (lambda () #f)
           (lambda ()
             (match-define (list _ status text headers) (regexp-match #rx"^HTTP/1\\.1 ([0-9][0-9][0-9]) ([^\n\r]*)(.*)" hdrs))
             (when (not (equal? status "200"))
               (raise (exn:fail (format "Error when fetching template ~a: ~a ~a." source status text)
                                (current-continuation-marks))))
             (when (not (equal? (string-trim (extract-field "Content-Type" headers)) "application/zip"))
               (raise (exn:fail (format "Error when fetching template ~a: template was not a ZIP file." source)
                                (current-continuation-marks))))
             (thunk port))
           (lambda () (close-input-port port)))])]
    [else
      (call-with-input-file source thunk)]))
