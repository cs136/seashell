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
(require seashell/backend/project
         seashell/backend/template
         seashell/seashell-config
         seashell/log
         net/uri-codec
         net/base64
         json
         racket/contract
         racket/port
         racket/match
         racket/file
         racket/path
         racket/date
         racket/generator
         racket/string
         racket/function
         file/unzip
         openssl/md5)

;(provide restore-file-from-template)

;; (restore-file-from-template project file template)
;; Restores a file from a skeleton/template.
;;
;; Args:
;;  project - Project.
;;  file - Path to file.
;;  template - Path (possibly URL) to template.
;; Returns:
;;  MD5 hash of file.
;(define/contract (restore-file-from-template project file template)
;  (-> (and/c project-name? is-project?) path-string? (or/c path-string? url-string?) string?)
;  (define ok #f)
;  (define-values (question-dir filename _) (split-path file))
;  (define dest-dir (check-and-build-path (build-project-path project) question-dir))
;  (make-directory* dest-dir)
;  (define destination (check-and-build-path dest-dir filename))
;  (define source (explode-path file))
;  (call-with-template template
;                      (lambda (port)
;                        (unzip port
;                               (lambda (name _2 contents)
;                                 (define lname (explode-path (simplify-path (bytes->path name) #f)))
;                                 (when (and (not (null? lname))
;                                            (equal? source (cdr lname)))
;                                   (call-with-write-lock (thunk
;                                     (call-with-output-file destination
;                                                            (lambda (dport)
;                                                              (define-values (md5in md5out) (make-pipe))
;                                                              (copy-port contents dport md5out)
;                                                              (close-output-port md5out)
;                                                              (set! ok (md5 md5in)))
;                                                            #:exists 'replace))))))))
;  (when (not ok)
;    (raise (exn:fail (format "File ~a (~a) not found in template ~a!" file source template)
;           (current-continuation-marks))))
;  ok)
