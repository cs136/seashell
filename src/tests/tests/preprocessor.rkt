#lang racket
;; Seashell Preprocessor Tests
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

;; (get-co-files/rec main-file file-dir common-dir
;; Produces a list of the user's compilation files, recursively resolving
;; dependencies
;;
;; Arguments:
;;  c-files    - The .c files being compiled
;;  file-dir   - The directory containing main-file
;;  common-dir - The directory containing the common subdirectory
;;
;; Returns:
;;  A list of the .h files included by a program, with the .h extension stripped
(define/contract (get-co-files/rec c-files o-files file-dir common-dir depth)
  (-> (listof path-string?) (listof path-string?) path? path? exact-nonnegative-integer? 
      (values (listof path-string?) (listof path-string?)))

  (logf 'debug "c-files is ~s\n" c-files)
  (logf 'debug "o-files is ~s\n" o-files)

  (define headers (get-headers c-files file-dir common-dir))
  (logf 'debug "Header files are ~s." headers)

  (define-values (found-c-files found-o-files) (get-co-files headers))
  (logf 'debug ".c files are ~s." found-c-files)
  (logf 'debug ".o files are ~s." found-o-files)

  (cond
    ;; TODO: off by one on depth? subset? works w.r.t. path-string? vs string?
    [(or (> depth (read-config 'header-search-depth)) 
         (and (subset? found-c-files c-files)
              (subset? found-o-files o-files))) 
     (values c-files o-files)]
    [else 
      (get-co-files/rec (remove-duplicates (append c-files found-c-files)) 
                       (remove-duplicates (append o-files found-o-files))
                       file-dir common-dir (add1 depth))]))


;; (get-headers c-files file-dir common-dir)
;; Produces a list of the user's header files included by the files in c-files (without recursively
;; resovling dependencies
;;
;; Arguments:
;;  c-files    - The .c files being compiled
;;  file-dir   - The directory containing main-file
;;  common-dir - The directory containing the common subdirectory
;;
;; Returns:
;;  A list of the .h files included by the files in c-files, with the .h extension stripped
;; Raises:
;;  exn:project if an included header is not a .h file
;; TODO: need to clean up the subprocess and ports?
(define/contract (get-headers c-files file-dir common-dir)
  (-> (listof path-string?) path? path? (listof path-string?))
  (define clang-error (open-output-file "/dev/null" #:exists 'truncate))
  (define-values (clang clang-output clang-input fake-error)
    ;; TODO: is 'system-linker the right binary?
    (apply subprocess #f #f clang-error (read-config 'system-linker) `("-E" ,@c-files "-I" ,common-dir)))
  (define files
    (remove-duplicates
      (filter values
        (for/list ([line (in-lines clang-output)])
          (match (regexp-match #rx"^# [0-9]+ \"([^<][^\"]*)\"" line)
            [(list _ file)
              (match-define-values (hdrpath hdrname _) (split-path file))
              (cond
                [(and (or (equal? (path->directory-path hdrpath) (path->directory-path file-dir))
                          (equal? (path->directory-path hdrpath) (path->directory-path common-dir)))
                      (regexp-match #rx"\\.h$" hdrname))
                  (substring file 0 (- (string-length file) 2))]
                [else #f])]
            [#f #f])))))
  (close-input-port clang-output)
  (close-output-port clang-input)
  (close-output-port clang-error)
  files)

;; (get-co-files headers)
;; Produces a list of the local .c and .o files to be compiled/linked with a program
;;
;; Arguments:
;;  headers - The list of included local .h files, i.e., produced by get-headers
;;
;; Returns:
;;  A list of the .c files and a list of the .o files to be compiled/linked with a program.
;; Raises:
;;  exn:project if an element of headers is not a .h file, if a .h file has no
;;  corresponding .o or .c file, or if a .h file has both a .c and .o file.
(define/contract (get-co-files headers)
  (-> (listof path-string?) (values (listof path?) (listof path?)))

  ;; TODO: object/c files must be in the same directory as the header
  (logf 'debug "headers in get-co-files: ~s\n" headers)
  (for/fold ([c-files '()]
             [o-files '()])
            ([hdr headers])
    (match-define-values (basedir hdrname _) (split-path hdr))
    (match/values (values (file-exists? (string-append hdr ".c"))
                          (file-exists? (string-append hdr ".o")))
      [(#t #t) (raise (exn:project (format "You included ~a.h, but provided both ~a.c and ~a.o"
                                            hdrname hdrname hdrname)
                                   (current-continuation-marks)))]
      [(#t #f) (values (cons (string->path (string-append hdr ".c")) c-files) o-files)]
      [(#f #t) (values c-files (cons (string->path (string-append hdr ".o")) o-files))]
      [(#f #f) (raise (exn:project (format "You included ~a.h, but did not provide ~a.c or ~a.o"
                                          hdrname hdrname hdrname)
                                   (current-continuation-marks)))])))

#| Invocation for the old-style preprocessor:
;; Get the .c and .o files needed to compile file
(define-values (c-files o-files)
  (get-co-files/rec (list (build-path base exe)) '() base project-common 0))
|#
