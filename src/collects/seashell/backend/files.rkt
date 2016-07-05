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
         file/unzip
         openssl/md5)

(provide (struct-out exn:project:file)
         (struct-out exn:project:file:checksum)
         new-file
         new-directory
         remove-file
         remove-directory
         read-file
         write-file
         list-files
         rename-file
         restore-file-from-template
         read-settings
         write-settings)

(struct exn:project:file exn:project ())
(struct exn:project:file:checksum exn:project:file ())

;; (new-file project file) -> void?
;; Creates a new file inside a project.
;;
;; Arguments:
;;  project - project to create file in.
;;  file - name of new file.
;;  normalize - boolean, whether or not to convert newlines to Unix
;;
;; Returns:
;;  MD5 checksum of file written.
;;
;; Raises:
;;  exn:project:file if file exists.
(define/contract (new-file project file contents encoding normalize?)
  (-> (and/c project-name? is-project?) path-string? bytes? (or/c 'raw 'url) boolean? string?)
  (define path (check-and-build-path (build-project-path project) file))
  (with-handlers
    [(exn:fail:filesystem?
       (lambda (exn)
         (raise (exn:project:file
                  (format "File already exists, or some other filesystem error occurred: ~a" (exn-message exn))
                  (current-continuation-marks)))))]
    (define data
      (with-input-from-bytes contents
        (lambda () 
          (cond
            [(eq? encoding 'url)
             (match (regexp-match #rx"^data:([^;]*)?(?:;(?!base64)([^;]*))?(?:;(base64))?," (current-input-port))
              [#f #""]
              [(list _ mime charset b64?)
               ;; Apparently Safari sometimes sets the mime type to 'base64'...
               ;; (data:base64, ....)
               (if (or b64? (bytes=? mime #"base64"))
                 (base64-decode (port->bytes))
                 (string->bytes/utf-8 (uri-decode (port->string))))])]
            [else
              ;; no-op (ignore port)
              contents]))))
    (define to-write
      (if normalize?
        (with-output-to-bytes
          (lambda ()
            (display-lines (call-with-input-bytes data port->lines))))
        data))
    (with-output-to-file path (lambda () (write-bytes to-write)) #:exists 'error)
    (call-with-input-bytes to-write md5)))


(define/contract (new-directory project dir)
  (-> (and/c project-name? is-project?) path-string? void?)
  (with-handlers
    [(exn:fail:filesystem?
      (lambda (exn)
        (raise (exn:project:file
          (format "Directory already exists, or some other filesystem error occurred: ~a" (exn-message exn))
          (current-continuation-marks)))))]
    (make-directory*
      (check-and-build-path (build-project-path project) dir))
    (void)))

;; (remove-file project file) -> void?
;; Deletes a file inside a project.
;;
;; Arguments:
;;  project - project to delete file from.
;;  file - name of file to delete.
;;
;; Raises:
;;  exn:project:file if file does not exist.
(define/contract (remove-file project file [tag #f])
  (->* ((and/c project-name? is-project?) path-string?) ((or/c string? #f)) void?)
  (define file-to-write (check-and-build-path (build-project-path project) file))
  (define history-path (get-history-path file-to-write))
  (with-handlers
    [(exn:fail:filesystem?
       (lambda (exn)
         (raise (exn:project:file
                  (format "File does not exists, or some other filesystem error occurred: ~a" (exn-message exn))
                  (current-continuation-marks)))))]
    (call-with-file-lock/timeout file-to-write 'exclusive
                                 (lambda ()
                                   (when tag
                                     (define expected-tag (call-with-input-file file-to-write md5))
                                     (unless (equal? tag expected-tag)
                                       (raise (exn:project:file:checksum
                                                (format "Could not write delete file ~a! (file changed on disk)" (some-system-path->string file-to-write))
                                                        (current-continuation-marks)))))
                                    (logf 'info "Deleting file ~a!" (some-system-path->string file-to-write))
                                    (delete-file file-to-write))
                                 (lambda ()
                                   (raise (exn:project:file
                                          (format "Could not delete file ~a! (file locked)" (some-system-path->string file-to-write))
                                                  (current-continuation-marks))))))
  (when (file-exists? history-path)
    (delete-file history-path))
  (void))

;; (remove-directory project dir)
;; Deletes the directory at the given path dir, INCLUDING all files in the
;; directory.
(define/contract (remove-directory project dir)
  (-> (and/c project-name? is-project?) path-string? void?)
  (with-handlers
    [(exn:fail:filesystem?
      (lambda (exn)
        (raise (exn:project:file
          (format "Filesystem error occurred: ~a" (exn-message exn))
          (current-continuation-marks)))))]
    (logf 'info "Deleting directory ~a!" (some-system-path->string (check-and-build-path (build-project-path project) dir)))
    (delete-directory/files (check-and-build-path (build-project-path project) dir)))
  (void))

;; (read-file project file) -> bytes?
;; Reads a file and its undoHistory as Racket bytestrings.
;;
;; Arguments:
;;  project - project to read file from.
;;  file - name of file to read.
;;
;; Returns:
;;  Contents of the file as a bytestring, and the MD5 checksum of the file, and the history.
(define/contract (read-file project file)
  (-> (and/c project-name? is-project?) path-string? (values bytes? string? bytes?))
  (define data (with-input-from-file (check-and-build-path (build-project-path project) file)
                                      port->bytes))
  (define history-path (get-history-path (check-and-build-path (build-project-path project) file)))
  (define undo-history-data (if (file-exists? history-path)
                                (with-input-from-file history-path port->bytes)
                                #""))
  (values data (call-with-input-bytes data md5) undo-history-data))

;; (write-file project file contents) -> string?
;; Writes a file from a Racket bytestring.
;;
;; Arguments:
;;  project - project.
;;  file - name of file to write.
;;  contents - contents of file.
;;  tag - MD5 of expected contents before file write, or #f
;;        to force write.
;;  history - History of file.
;; Returns:
;;  MD5 checksum of resulting file.
(define/contract (write-file project file contents [history #f] [tag #f])
  (->* ((and/c project-name? is-project?) path-string? bytes?)
       ((or/c #f bytes?) (or/c #f string?))
       string?)
  (define file-to-write (check-and-build-path (build-project-path project) file))
  (call-with-file-lock/timeout file-to-write 'exclusive
                               (lambda ()
                                 (when tag
                                   (define expected-tag (call-with-input-file file-to-write md5))
                                   (unless (equal? tag expected-tag)
                                     (raise (exn:project:file:checksum
                                              (format "Could not write to file ~a! (file changed on disk)" (some-system-path->string file-to-write))
                                                      (current-continuation-marks)))))
                                 (with-output-to-file (check-and-build-path (build-project-path project) file)
                                                      (lambda () (write-bytes contents))
                                                      #:exists 'must-truncate)
                                 (when history
                                   (with-output-to-file (get-history-path (check-and-build-path (build-project-path project) file))
                                                        (lambda () (write-bytes history))
                                                        #:exists 'replace)))
                               (lambda ()
                                 (raise (exn:project:file
                                          (format "Could not write to file ~a! (file locked)" (some-system-path->string file-to-write))
                                                  (current-continuation-marks)))))
  (call-with-input-bytes contents md5))

;; (get-history-path path) -> path
;; Given a file's path, returns the path to that file's corresponding .history file
;;
;; Arguments:
;;  path - path to file
;;
;; Returns:
;;  path to file's undoHistory (stored as json string)
(define (get-history-path path)
  (define-values (base name _1) (split-path (simplify-path path)))
  (define history-file (string->path (string-append "." (path->string name) ".history")))
  (build-path base history-file))

;; (list-files project)
;; Lists all files and directories in a project.
;;
;; Arguments:
;;  project - Project to deal with.
;;  dir - optional, subdirectory within project to start at.
;;      Mainly used for recursive calls.
;; Returns:
;;  (listof (string? boolean? number? string?)) - Files and directories in a project
;;            \------|--------|-------|---------  Name.
;;                   \--------|-------|---------  Is directory?
;;                            \-------|---------  Last modification time.
;;                                    \---------  Checksum, if file.
(define/contract (list-files project [dir #f])
  (->* ((and/c project-name? is-project?))
    ((or/c #f (and/c string? path-string?)))
    (listof (list/c (and/c string? path-string?) boolean? number? (or/c #f string?))))
  (define start-path (if dir (check-and-build-path
    (build-project-path project) dir) (build-project-path project)))
  (foldl (lambda (path rest)
    (define current (build-path start-path path))
    (define relative (if dir (build-path dir path) path))
    (define modified (* (file-or-directory-modify-seconds current) 1000))
    (cond
      [(and (directory-exists? current) (not (file-or-directory-hidden? current)))
        (cons (list (some-system-path->string relative) #t modified #f)
              (append (list-files project relative) rest))]
      [(and (file-exists? current) (not (file-or-directory-hidden? current)))
       ; directory-hidden should work for files as well (can rename if so)
        (cons (list (some-system-path->string relative) #f modified
                    (call-with-input-file current md5)) rest)]
      [else rest]))
    '() (directory-list start-path)))

;; Determines if a file/directory is hidden (begins with a .)
(define/contract (file-or-directory-hidden? path)
  (-> path? boolean?)
  (define-values (_1 filename _2) (split-path (simplify-path path)))
  (string=? "." (substring (some-system-path->string filename) 0 1)))

;; (rename-file project old-file new-file)
;; Renames a file.
;;
;; Args:
;;  project - Project the file is in
;;  old-file - The original name of the file
;;  new-file - The desired new name of the file
;; Returns:
;;  void?
(define/contract (rename-file project old-file new-file)
  (-> (and/c project-name? is-project?) path-string? path-string? void?)
  (define proj-path (check-and-build-path (build-project-path project)))
  (with-handlers
    [(exn:fail:filesystem? (lambda (e)
      (raise (exn:project "File could not be renamed." (current-continuation-marks)))))]
    (unless (equal? old-file new-file)
      (rename-file-or-directory (check-and-build-path proj-path old-file)
                                (check-and-build-path proj-path new-file)))))

;; (restore-file-from-template project file template)
;; Restores a file from a skeleton/template.
;;
;; Args:
;;  project - Project.
;;  file - Path to file.
;;  template - Path (possibly URL) to template.
;; Returns:
;;  MD5 hash of file.
(define/contract (restore-file-from-template project file template)
  (-> (and/c project-name? is-project?) path-string? (or/c path-string? url-string?) string?)
  (define ok #f)
  (define-values (question-dir filename _) (split-path file))
  (define dest-dir (check-and-build-path (build-project-path project) question-dir))
  (make-directory* dest-dir)
  (define destination (check-and-build-path dest-dir filename))
  (define source (explode-path file))
  (call-with-template template
                      (lambda (port)
                        (unzip port
                               (lambda (name _2 contents)
                                 (define lname (explode-path (simplify-path (bytes->path name) #f)))
                                 (when (and (not (null? lname))
                                            (equal? source (cdr lname)))
                                   (call-with-output-file destination
                                                          (lambda (dport)
                                                            (define-values (md5in md5out) (make-pipe))
                                                            (copy-port contents dport md5out)
                                                            (close-output-port md5out)
                                                            (set! ok (md5 md5in)))
                                                          #:exists 'replace))))))
  (when (not ok)
    (raise (exn:fail (format "File ~a (~a) not found in template ~a!" file source template))
           (current-continuation-marks)))
  ok)

;; (write-settings font-size editor-mode tab-width use-spaces)
;; Writes the user's seashell settings to ~/.seashell/settings.txt
;;
;; Arguments:
;;  settings - JSON object representing the user's settings
(define/contract (write-settings settings)
  (-> jsexpr? void?)
  (with-output-to-file (build-path (read-config 'seashell) "settings.txt")
    (lambda () (write settings)) #:exists 'truncate))

;; (read-settings)
;; Reads the user's seashell settings from ~/.seashell/settings.txt. If the
;; file does not exist, a new file is created with the default settings, and
;; its contents are used.
;;
;; Returns:
;;  settings - JSON object representing the user's settings, or "notexists" if
;;             the settings file doesn't exist
(define/contract (read-settings)
  (-> jsexpr?)
  (cond
    [(file-exists? (build-path (read-config 'seashell) "settings.txt"))
      (with-input-from-file (build-path (read-config 'seashell) "settings.txt")
        (lambda () (read)))]
    [else #f]))


