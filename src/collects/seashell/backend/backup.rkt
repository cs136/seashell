#lang racket/base

(require seashell/backend/project
         seashell/backend/files
         racket/contract
         racket/port
         racket/file
         racket/path
         racket/generator
         racket/string
         racket/set)

(provide list-backups
         write-backup
         restore-from-backup
         write-backup-if-changed
         get-backup-path
         clean-backups)

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
  (define relative-backups-path (cadr (string-split (path->string full-backups-path)
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
  (define-values (file-contents _1 file-undo-history) (read-file project file))
  (define most-recent-backup (if (not (null? backups))
                               (car backups)
                               null))
  (define-values (backup-contents _2 backup-history)
                 (if (not (null? backups))
                     (read-file project most-recent-backup)
                     (values #"" "" #"")))
  (when (or (equal? most-recent-backup null)
            (not (equal? backup-contents file-contents)))
        (write-backup project file))
  (void))

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

;; (clean-backups project file num num num) -> void
;; given a path to a file, "cleans up" the file's corresponding directory of backups
;; by keeping daily-keep number of backups from the past 24 hours,
;; weekly-keep number of backups from the past week, and
;; monthly-keep number of backups from the past 30 days
;;
;; Arguments:
;;  project - a project
;;  file - a file
;;  daily - number of backups to keep from the past 24 hours
;;  weekly - number of backups to keep from the past week
;;  monthly - number of backups to keep from the past 30 days
(define/contract (clean-backups project file monthly-keep weekly-keep daily-keep)
  (-> (and/c project-name? is-project?) path-string? number? number? number? void?)
  ;; removes num items from lst, evenly distributed (i.e. removing 2 items will remove from the beginning and middle)
  (define (list-evenly-remove lst num)
    (define limit (ceiling (/ (length lst) num)))
    (define (list-evenly-remove/acc lst counter limit)
      (cond [(equal? 0 limit) lst]
            [(null? lst) '()]
            [(equal? counter 0)
             (list-evenly-remove/acc (cdr lst) (sub1 limit) limit)]
            [else (cons (car lst)
                  (list-evenly-remove/acc (cdr lst) (sub1 counter) limit))]))
    (list-evenly-remove/acc lst 0 limit))

  ;; creates a set from the list lst
  ;; (works like list->set)
  (define (list->set lst)
    (define (list->set/acc lst st)
      (cond [(null? lst) st]
            [else (list->set/acc (cdr lst) (set-add st (car lst)))]))
    (list->set/acc lst (set)))
    
  (define full-backups-path (get-backup-path project file))
  (define relative-backups-path (cadr (string-split (path->string full-backups-path)
                                                      (string-append project "/"))))
  (define unsorted-backups (list-files project relative-backups-path))
  (define sorted-backups (sort unsorted-backups (lambda (x y) (> (caddr x) (caddr y)))))
  (define now (* 1000 (current-inexact-milliseconds)))
  (define millis-in-day 86400000)
  (define millis-in-week (* millis-in-day 7))
  (define millis-in-month (* millis-in-day 30))
  (define daily-backups   (filter (lambda (x) (< (caddr x) (- now millis-in-day))) sorted-backups)) ; all backups less than a day old
  (define num-daily-delete (max 0 (- (length daily-backups) daily-keep))) ; if we want to keep >= the number of backups we have, don't delete any
  (define daily-keep-list (list-evenly-remove daily-backups num-daily-delete))
  (define weekly-backups  (filter (lambda (x) (and (< (caddr x) (- now millis-in-week)) ; all backups less than a week old, but more than a day
                                                   (not (member daily-keep-list x)))) sorted-backups))
  (define num-weekly-delete (max 0 (- (length weekly-backups) weekly-keep))) 
  (define weekly-keep-list (list-evenly-remove weekly-backups num-weekly-delete))
  (define monthly-backups (filter (lambda (x) (and (< (caddr x) (- now millis-in-month)) ; all backups less than 30 days old, but more than a week
                                                   (not (member daily-keep-list x))
                                                   (not (member weekly-keep-list x)))) sorted-backups))
  (define num-monthly-delete (max 0 (- (length monthly-backups) monthly-keep)))
  (define monthly-keep-list (list-evenly-remove monthly-backups num-monthly-delete))
  (define all-keep-set (list->set (append daily-keep-list weekly-keep-list monthly-keep-list)))
  (for-each (lambda (x) (unless (set-member? all-keep-set x) (delete-file (check-and-build-path full-backups-path (cadr (string-split (car x)"/"))))))
              ;(printf "~a\n" (cadr (string-split (car x)"/"))))
            sorted-backups)
  (void))
