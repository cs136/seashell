#lang racket
;; Seashell's backend server.
;; Copyright (C) 2013 The Seashell Maintainers.
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
         seashell/seashell-config)

(provide exn:project:file new-file delete-file read-file write-file list-files)

(struct exn:project:file exn:project ())

;; (new-file project file) -> void?
;; Creates a new file inside a project.
;;
;; Arguments:
;;  project - project to create file in.
;;  file - name of new file.
;;
;; Raises:
;;  exn:project:file if file exists.
(define/contract (new-file project file)
  (-> (and/c project-name? is-project?) path-string? void?)
  (with-handlers
    [(exn:fail:filesystem?
       (lambda (exn)
         (raise (exn:project
                  (format "File already exists, or some other filesystem error occurred: ~a" (exn-message exn))
                  (current-continuation-marks)))))]
    (close-output-port (open-output-file
                         (check-and-build-path (read-config 'seashell) project file)
                         #:exists 'append)))
  (void))

;; (delete-file project file) -> void?
;; Deletes a file inside a project.
;;
;; Arguments:
;;  project - project to delete file from.
;;  file - name of file to delete.
;;
;; Raises:
;;  exn:project:file if file does not exist.
(define/contract (delete-file project file)
  (-> (and/c project-name? is-project?) path-string? void?)
  (with-handlers
    [(exn:fail:filesystem?
       (lambda (exn)
         (raise (exn:project
                  (format "File does not exists, or some other filesystem error occurred: ~a" (exn-message exn))
                  (current-continuation-marks)))))]
    (delete-file (check-and-build-path (read-config 'seashell) project file)))
  (void))

;; (read-file project file) -> bytes?
;; Reads a file as a Racket bytestring.
;;
;; Arguments:
;;  project - project to read file from.
;;  file - name of file to read.
;;
;; Returns:
;;  Contents of the file as a bytestring.
(define/contract (read-file project file)
  (-> (and/c project-name? is-project?) path-string? bytes?)
  (with-input-from-file (check-and-build-path (read-config 'seashell) project file)
                        (lambda () (read-bytes 0))))

;; (write-file project file contents) -> void?
;; Writes a file from a Racket bytestring.
;;
;; Arguments:
;;  project - project.
;;  file - name of file to write.
;;  contents - contents of file.
(define/contract (write-file project file contents)
  (-> (and/c project-name? is-project?) path-string? bytes? void?)
  (with-output-to-file (check-and-build-path (read-config 'seashell) project file)
                       (lambda () (write-bytes contents))
                       #:exists 'truncate/replace)
  (void))

;; (list-files project)
;; Lists all files in a project.
;;
;; Arguments:
;;  project - Project to deal with.
;; Returns:
;;  (listof string?) - Files in project.
;;
;; Notes:
;;  This function assumes that projects are organized in a flat manner.
;;  We will have to rework Seashell if this assumption does not hold in the future.
(define/contract (list-files project)
  (-> (and/c project-name? is-project?) (listof (and/c string? path-string?)))
  (define project-path (check-and-build-path (read-config 'seashell) project))
  (map path->string
    (filter
      (lambda (path)
        (file-exists? (build-path project-path path)))
      (directory-list project-path))))
