#lang racket/base

(require seashell/backend/project
         seashell/backend/files
         racket/contract
         racket/port
         racket/file
         racket/path
         racket/generator
         racket/string)

(provide list-backups
         write-backup
         restore-from-backup
         write-backup-if-changed
         get-backup-path)

;; (list-backups project file)
;; Lists all backups corresponding to a project+file. Sorted with the most recent first.
;; Arguments:
;;  project - project/assignment containing the file you're looking for
;;  file - question/file you want to find backups of
;;
;; Returns:
;;  (listof string?) - backup copies of the given file (as path-strings)
(define/contract (list-backups project file)
  (-> (and/c project-name? is-project?) (and/c string? path-string?)
      (listof (and/c string? path-string?)))
  (define full-backups-path (get-backup-path project file))
  (define (second lst) (car (cdr lst)))
  (define relative-backups-path (second (string-split (path->string full-backups-path)
                                                      (string-append project "/"))))
  (define unsorted-backups (list-files project relative-backups-path))
  (define sorted-backups (sort unsorted-backups (lambda (x y) (> (caddr x) (caddr y)))))
  (map car sorted-backups))

;; unique number generator for backup filenames
(define num-gen (sequence->generator (in-naturals 0)))
;; (write-backup project file) -> void?
;; creates a backup of a file in a hidden folder
;;
;;  project - project.
;;  file - name of file to write.
(define/contract (write-backup project file)
  (-> (and/c project-name? is-project?) path-string? void?)
  (define source (check-and-build-path (build-project-path project) file))
  (define backup-folder (get-backup-path project file))
  (define timestamp (number->string (num-gen)))
  (define destination (check-and-build-path backup-folder (string-append "backup_" timestamp)))
  (when (not (directory-exists? backup-folder)) (make-directory backup-folder))
  (copy-file source destination #t)
  (void))

;; (restore-from-backup project file backup) -> void?
;; replaces file with backup, overwriting file's contents
;;
;; project - project
;; file - file to overwrite (destination)
;; backup - a backup of file (source)
(define/contract (restore-from-backup project file backup)
  (-> (and/c project-name? is-project?) path-string? path-string? void?)
  (define dest (check-and-build-path (build-project-path project) file))
  (define src (check-and-build-path (build-project-path project) backup))
  (delete-file dest)
  (copy-file src dest #t)
  (void))

;; (write-backup-if-changed project file) -> void
;; calls write-backup, but only if the file currently is different from the most recent backup
(define/contract (write-backup-if-changed project file)
  (-> (and/c project-name? is-project?) path-string? void?)
  (define project-path (build-project-path project))
  (define backups (if (directory-exists? (get-backup-path project file))
                      (list-backups project file)
                      null))
  (define-values (file-contents file-undo-history) (read-file project file))
  (define most-recent-backup (if (not (null? backups))
                               (car backups)
                               null))
  (define-values (backup-contents backup-history)
                 (if (not (null? backups))
                     (read-file project most-recent-backup)
                     (values #"" #"")))
  (when (or (equal? most-recent-backup null)
            (not (equal? backup-contents file-contents)))
        (write-backup project file))
  (void))

;; TODO: maybe combine "get-X-path" into something more general?
;; (get-backup-path path) -> path
;; given a file's path, returns the path to that file's backups folder
;; Arguments:
;;  path - path to file
;;
;; Returns:
;;  path to file's backups
(define (get-backup-path project file)
  (define-values (base name _1) (split-path (simplify-path (check-and-build-path (build-project-path project) file))))
  (define backup-folder (string->path (string-append "." (path->string name) "_backup")))
  (build-path base backup-folder))

(define/contract (clean-backups project file monthly weekly daily)
  (-> (and/c project-name? is-project?) path-string? number? number? number? void?)
  ;; when cleaning:
  ;; build a set for (m monthly, w weekly, y daily) backup filenames)
  ;; if something is in the set, don't delete it
  ;; else delete it
  )
