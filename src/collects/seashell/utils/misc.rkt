#lang typed/racket
;; Seashell's compiler system.
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
(provide check-eof merge-directory/files)

;; Handy syntax rule for EOF checking
(: check-eof (All (X) (-> (U EOF X) X)))
(define (check-eof x)
  (cond
    [(eof-object? x)
      (raise (exn:fail (format "read: Unexpected end of file!")
                               (current-continuation-marks)))]
    [else
      x]))

(: raise-not-a-file-or-directory (-> Any Path-String Nothing))
(define (raise-not-a-file-or-directory who path)
  (raise
   (make-exn:fail:filesystem
    (format "~a: encountered path that is neither file nor directory\n  path: ~a"
            who
            path)
    (current-continuation-marks))))

(: merge-directory/files (->* (Path-String Path-String)
                              (#:keep-modify-seconds? Boolean
                               #:preserve-links? Boolean
                               #:overwrite? Boolean)
                              Any))
(define (merge-directory/files src dest
                              #:keep-modify-seconds? [keep-modify-seconds? #f]
                              #:preserve-links? [preserve-links? #f]
                              #:overwrite? [overwrite? #f])
  (let loop ([src src] [dest dest])
    (begin
      (when overwrite?
        (cond [(or (file-exists? dest) (link-exists? dest))
               (delete-file dest)]
              [(directory-exists? dest) (delete-directory/files dest)]))
      (when (not (or (file-exists? dest) (link-exists? dest) (directory-exists? dest)))
        (cond [(and preserve-links?
                    (link-exists? src))
               (make-file-or-directory-link
                (resolve-path src)
                dest)]
              [(file-exists? src)
               (copy-file src dest)
               (when keep-modify-seconds?
                 (file-or-directory-modify-seconds
                  dest
                  (file-or-directory-modify-seconds src)))]
              [(directory-exists? src)
               (when (not (directory-exists? dest))
                (make-directory dest))
               (for-each (lambda ([e : Path])
                           (loop (build-path src e)
                                 (build-path dest e)))
                         (directory-list src))]
              [else (raise-not-a-file-or-directory 'merge-directory/files src)])))))
