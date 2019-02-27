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
         seashell/backend/lock
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
         file/gzip
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

(define/contract (call-with-file-lock/delete-lock-file filename kind tnk failure-thunk)
  (-> path-string? (or/c 'shared 'exclusive) (-> any) (-> any) any)
  (call-with-file-lock/timeout
    filename kind
    (thunk (dynamic-wind
      (thunk (void))
      tnk
      (thunk (with-handlers ([exn:fail:filesystem? void])
        (delete-file (make-lock-file-name filename))))))
    failure-thunk))

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
(define/contract (new-file project file contents encoding normalize? [history #f])
  (->* ((and/c project-name? is-project?) path-string? bytes? (or/c 'raw 'url) boolean?) ((or/c #f bytes?)) string?)
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
    (call-with-write-lock
      (thunk (with-output-to-file path (lambda () (write-bytes to-write)) #:exists 'error)))
    (call-with-input-bytes to-write md5)))


(define/contract (new-directory project dir)
  (-> (and/c project-name? is-project?) path-string? void?)
  (with-handlers
    [(exn:fail:filesystem?
      (lambda (exn)
        (raise (exn:project:file
          (format "Directory already exists, or some other filesystem error occurred: ~a" (exn-message exn))
          (current-continuation-marks)))))]
    (call-with-write-lock
      (thunk (make-directory*
        (check-and-build-path (build-project-path project) dir))))
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
  (call-with-write-lock
    (thunk (with-handlers
      [(exn:fail:filesystem?
         (lambda (exn)
           (raise (exn:project:file
                    (format "File does not exists, or some other filesystem error occurred: ~a" (exn-message exn))
                    (current-continuation-marks)))))]
      (call-with-file-lock/delete-lock-file file-to-write 'exclusive
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
      (delete-file history-path))))
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
    (call-with-write-lock (thunk
      (logf 'info "Deleting directory ~a!" (some-system-path->string (check-and-build-path (build-project-path project) dir)))
      (delete-directory/files (check-and-build-path (build-project-path project) dir)))))
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
  (define file-path (check-and-build-path (build-project-path project) file))
  (define data (with-input-from-file file-path port->bytes))
  (define history-path (get-history-path file-path))
  (define undo-history-data (if (file-exists? history-path)
                                (with-input-from-file history-path port->bytes)
                                #""))
  (define md5-hash (call-with-input-bytes data md5))

  ;; write file contents to a log
  (with-handlers ([exn:fail? (lambda (exn) (logf 'error "(read-file) Error occurred writing to gzip log: ~a" exn))])
    (define log-separator (string->bytes/utf-8 (format "\n\n[~a] read-file ~a ~a\n" (date->string (current-date) #t) file-path md5-hash)))
    (define input-bytes-port (open-input-bytes (bytes-append log-separator data)))
    (define-values (base name _unused) (split-path file-path))
    (define output-bytes-port (open-output-file (build-path base (string->path (string-append "." (path->string name) ".log")))
                                                #:mode 'binary #:exists 'append))
    (logf 'info "(read-file) Writing gzip log for ~a (md5 hash ~a)" file-path md5-hash)
    (gzip-through-ports input-bytes-port output-bytes-port (path->string name) (date->seconds (current-date)))
    (close-input-port input-bytes-port)
    (close-output-port output-bytes-port))

  (values data md5-hash undo-history-data))

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
  ;; Helper function to atomically update a file. Same as the built-in call-with-atomic-output-file
  ;; except this writes to a hidden temporary file so that students won't see it.
  (define (call-with-atomic-output-file-hidden path
                                        proc
                                        #:security-guard [guard #f])
    (unless (path-string? path)
      (raise-argument-error 'call-with-atomic-output-file "path-string?" path))
    (unless (and (procedure? proc)
                 (procedure-arity-includes? proc 2))
      (raise-argument-error 'call-with-atomic-output-file "(procedure-arity-includes/c 2)" proc))
    (unless (or (not guard)
                (security-guard? guard))
      (raise-argument-error 'call-with-atomic-output-file "(or/c #f security-guard?)" guard))
    (define (try-delete-file path [noisy? #t])
      ;; Attempt to delete, but give up if it doesn't work:
      (with-handlers ([exn:fail:filesystem? void])
        (delete-file path)))
    (let ([bp (current-break-parameterization)]
          [tmp-path (parameterize ([current-security-guard (or guard (current-security-guard))])
                      (make-temporary-file ".tmp~a" #f (or (path-only path) (current-directory))))]
          [ok? #f])
      (dynamic-wind
       void
       (lambda ()
         (begin0
           (let ([out (parameterize ([current-security-guard (or guard (current-security-guard))])
                        (open-output-file tmp-path #:exists 'truncate/replace))])
             (dynamic-wind
              void
              (lambda ()
                (call-with-break-parameterization bp (lambda () (proc out tmp-path))))
              (lambda ()
                (close-output-port out))))
           (set! ok? #t)))
       (lambda ()
         (parameterize ([current-security-guard (or guard (current-security-guard))])
           (if ok?
               (if (eq? (system-type) 'windows)
                   (let ([tmp-path2 (make-temporary-file "tmp~a" #f (path-only path))])
                     (with-handlers ([exn:fail:filesystem? void])
                       (rename-file-or-directory path tmp-path2 #t))
                     (rename-file-or-directory tmp-path path #t)
                     (try-delete-file tmp-path2))
                   (rename-file-or-directory tmp-path path #t))
               (try-delete-file tmp-path)))))))

  (define file-to-write (check-and-build-path (build-project-path project) file))
  (call-with-write-lock (thunk
    (call-with-file-lock/delete-lock-file file-to-write 'exclusive
                                 (lambda ()
                                   (when tag
                                     (define expected-tag (call-with-input-file file-to-write md5))
                                     (unless (equal? tag expected-tag)
                                       (raise (exn:project:file:checksum
                                                (format "Could not write to file ~a! (file changed on disk)" (some-system-path->string file-to-write))
                                                        (current-continuation-marks)))))
                                   (call-with-atomic-output-file-hidden (check-and-build-path (build-project-path project) file)
                                                                        (lambda (temp-file-output-port temp-file-path) (write-bytes contents temp-file-output-port)))
                                   (when history
                                     (with-output-to-file (get-history-path (check-and-build-path (build-project-path project) file))
                                                          (lambda () (write-bytes  history))
                                                          #:exists 'replace)))
                                   ;; FOR TESTING ONLY - this should be done less often & have some logic to decide when
                                   ;(write-backup project file)
                                 (lambda ()
                                   (raise (exn:project:file
                                            (format "Could not write to file ~a! (file locked)" (some-system-path->string file-to-write))
                                                    (current-continuation-marks)))))))
  (define md5-hash (call-with-input-bytes contents md5))

  ;; write file contents to a log
  (with-handlers ([exn:fail? (lambda (exn) (logf 'error "(write-file) Error occurred writing to gzip log: ~a" exn))])
    (define log-separator (string->bytes/utf-8 (format "\n\n[~a] write-file ~a ~a\n" (date->string (current-date) #t) file-to-write md5-hash)))
    (define input-bytes-port (open-input-bytes (bytes-append log-separator contents)))
    (define-values (base name _unused) (split-path file-to-write))
    (define output-bytes-port (open-output-file (build-path base (string->path (string-append "." (path->string name) ".log")))
                                                #:mode 'binary #:exists 'append))
    (logf 'info "(write-file) Writing gzip log for ~a (md5 hash ~a)" file-to-write md5-hash)
    (gzip-through-ports input-bytes-port output-bytes-port (path->string name) (date->seconds (current-date)))
    (close-input-port input-bytes-port)
    (close-output-port output-bytes-port))

  md5-hash)

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
;;  string? - checksum of file
(define/contract (rename-file project old-file new-file)
  (-> (and/c project-name? is-project?) path-string? path-string? string?)
  (define proj-path (check-and-build-path (build-project-path project)))
  (with-handlers
    [(exn:fail:filesystem? (lambda (e)
      (raise (exn:project "File could not be renamed." (current-continuation-marks)))))]
    (unless (equal? old-file new-file)
      (call-with-write-lock (thunk
        (rename-file-or-directory (check-and-build-path proj-path old-file)
                                  (check-and-build-path proj-path new-file)))))
    (call-with-input-file (check-and-build-path proj-path new-file) md5)))

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
                                   (call-with-write-lock (thunk
                                     (call-with-output-file destination
                                                            (lambda (dport)
                                                              (define-values (md5in md5out) (make-pipe))
                                                              (copy-port contents dport md5out)
                                                              (close-output-port md5out)
                                                              (set! ok (md5 md5in)))
                                                            #:exists 'replace))))))))
  (when (not ok)
    (raise (exn:fail (format "File ~a (~a) not found in template ~a!" file source template)
           (current-continuation-marks))))
  ok)

;; (write-settings font-size editor-mode tab-width use-spaces)
;; Writes the user's seashell settings to ~/.seashell/settings.txt
;;
;; Arguments:
;;  settings - JSON object representing the user's settings
(define/contract (write-settings settings)
  (-> jsexpr? void?)
  (call-with-write-lock (thunk
    (with-output-to-file (build-path (read-config 'seashell) "settings.txt")
      (lambda () (write settings)) #:exists 'truncate))))

;; (read-settings)
;; Reads the user's seashell settings from ~/.seashell/settings.txt. If the
;; file does not exist, a new file is created with the default settings, and
;; its contents are used.
;;
;; Returns:
;;  settings - JSON object representing the user's settings, or "notexists" if
;;             the settings file doesn't exist
;;  last_modified - the time the settings file was last modified (in ms), or #f
;;                  if it does not exist
(define/contract (read-settings)
  (-> (values jsexpr? (or/c #f number?)))
  (define path (build-path (read-config 'seashell) "settings.txt"))
  (cond
    [(file-exists? path)
      (define res (with-input-from-file path read))
      (values (if (eof-object? res) #f res)
              (* 1000 (file-or-directory-modify-seconds path)))]
    [else (values #f #f)]))


