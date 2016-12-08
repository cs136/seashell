#lang typed/racket
;; Seashell
;; Copyright (C) 2016 The Seashell Maintainers
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
         typed/json
         seashell/utils/typed-json-struct
         typed/racket/date
         typed/racket/unsafe)

(provide sync-offline-changes)
(module untyped-helper racket/base
  (require racket/date)
  (provide date-nanosecond)
  (define (date-nanosecond dt)
    (date*-nanosecond dt)))
(unsafe-require/typed (submod "." untyped-helper)
                      [date-nanosecond (-> date Integer)])
(require/typed seashell/backend/project
               [project-base-path (-> Path)]
               [list-projects (-> (Listof (List String Integer)))]
               ;; specifying the return type of this as other than Any seems to cause problems
               [read-project-settings (-> String Any)]
               [write-project-settings (-> String Settings Void)]
               [build-project-path (-> String Path)]
               [check-path (-> Path Path)]
               [is-project? (-> String Boolean)]
               [#:struct (exn:project exn:fail:user) ()])
(require/typed seashell/backend/files
               [new-file (->* (String Path-String Bytes (U 'raw 'url) Boolean) ((U False Bytes)) String)]
               ;(->* ((and/c project-name? is-project?) path-string? bytes? (or/c 'raw 'url) boolean?) ((or/c #f string?)) string?)
               ;[new-file (-> String Path-String Bytes (U 'raw 'url) Boolean String)]
               [remove-file (->* (String Path-String) ((U False String)) Void)]
               [read-file (-> String Path-String (Values Bytes String Bytes))]
               [write-file (->* (String Path-String Bytes) ((U False Bytes) (U False String)) String)]
               [list-files (->* (String) ((U String False))
                                (Listof (List String Boolean Number (U False String))))]
               [read-settings (-> (Values Any Integer))]
               [write-settings (-> JSExpr Void)]
               [#:struct (exn:project:file exn:project) ()]
               [#:struct (exn:project:file:checksum exn:project:file) ()])

;; External datatypes (to send back to the frontend)
(json-struct off:project ([name : String]
                          [last_modified : Integer]
                          [settings : (Option String)]) #:transparent)
(json-struct off:file ([project : String] [file : String] [checksum : (Option String)]) #:transparent)
(json-struct off:change ([type : String] [file : off:file]
                         [contents : (Option String)] [checksum : (Option String)]
                         [history : (Option String)])
             #:transparent)
(json-struct off:settings ([modified : Integer] [values : String]) #:transparent)
(json-struct off:changes ([projects : (Listof off:project)]
                          [files : (Listof off:file)]
                          [changes : (Listof off:change)]
                          [settings : off:settings])
             #:transparent)
(json-struct off:conflict ([type : String]
                           [file : off:file]
                           [reason : String]
                           [saved? : Boolean])
             #:transparent)
(json-struct off:response ([conflicts : (Listof off:conflict)]
                           [changes : (Listof off:change)]
                           [newProjects : (Listof String)]
                           [deletedProjects : (Listof String)]
                           [updatedProjects : (Listof off:project)]
                           [settings : (Option String)])
             #:transparent)

;; Internal datatypes.
(define-type Conflict-Reason (U String 'checksum 'missing-directory))
(struct conflict ([type : String]
                  [project : String]
                  [file : String]
                  [contents : (Option String)]
                  [reason : Conflict-Reason]
                  [history : (Option Bytes)]) #:transparent #:type-name Conflict-Type)

(define-type Settings (HashTable Symbol (U String Number)))

;; Exception types.
(struct exn:project:sync exn:project ())

(define offline-path (build-path (read-config-path 'seashell) "offline"))

(: record-offline-changeset (-> Bytes JSExpr Any))
(define (record-offline-changeset timestamp js-changeset)
  (make-directory* offline-path)
  (with-output-to-file
    (build-path offline-path (format "changeset-~a.json" timestamp))
    (thunk
      (write-json js-changeset))))

(: reason->string (-> Conflict-Reason String))
(define (reason->string reason)
  (cond
    [(string? reason) (format "Exception in syncing: ~a." reason)]
    [(equal? reason 'checksum) "Checksum mismatch."]
    [(equal? reason 'missing-directory) "Could not create missing directory."]
    [else "Other reason."]))

(: make-timestamp (-> Bytes))
(define (make-timestamp)
  (parameterize ([date-display-format 'iso-8601])
    (define dt (current-date))
    (string->bytes/utf-8 (format "~a+~a" (date->string dt #t)
                                 (date-nanosecond dt)))))

(: apply-offline-changes (-> (Listof off:change) (Listof Conflict-Type)))
(define (apply-offline-changes their-changes)
  (foldl
    (lambda ([change : off:change]
             [conflicts : (Listof Conflict-Type)])
      : (Listof Conflict-Type)
      (with-handlers
        ([exn:fail?
           (lambda ([exn : exn])
             (define file (off:change-file change))
             (cons (conflict (off:change-type change)
                             (off:file-project file)
                             (off:file-file file)
                             (off:change-contents change)
                             (exn-message exn)
                             (if (off:change-history change) (string->bytes/utf-8 (off:change-history change)) #""))
                   conflicts))])
        (match change
          [(off:change "deleteFile"
                       (off:file project file checksum)
                       #f
                       off-checksum
                       _) ; deleting a file doesn't need history
           (assert (path-string? file))
           (cond
             ;; Missing project: ignore (delete on local, as cannot create project offline)
             [(not (is-project? project)) conflicts]
             [checksum
               (with-handlers
                 ([exn:project:file?
                    (lambda ([exn : exn])
                      (cons (conflict "deleteFile" project file #f 'checksum #f) conflicts))])
                 (remove-file project file checksum)
                 conflicts)]
             [else
               (raise (exn:project:sync "Expected non-#f checksum for deleteFile!"
                                        (current-continuation-marks)))])]
          [(off:change (and type (or "newFile" "editFile"))
                       (off:file project file _)
                       contents
                       checksum
                       history)
           (assert (path-string? file))
           [cond
             ;; Missing project: ignore (delete on local, as cannot create project offline)
             [(not (is-project? project)) conflicts]
             ;; Project present:
             [contents
               ;; Project present, create directory if missing.
               (define-values (base _1 _2) (split-path (check-path (build-path file))))
               ;; Attempt to write file.
               (: attempt-to-write-file (-> (Listof Conflict-Type)))
               (define (attempt-to-write-file)
                 (with-handlers
                   ([exn:project:file?
                      (lambda ([exn : exn])
                        (cons (conflict type project file contents 'checksum (if history (string->bytes/utf-8 history) #"")) conflicts))])
                   (cond
                     [(equal? type "editFile")
                       (write-file project file (string->bytes/utf-8 contents) #f checksum)]
                     [(equal? type "newFile")
                       (new-file project file (string->bytes/utf-8 contents) 'raw #f)])
                   conflicts))
               (if (path? base)
                 ;; Attempt to create base path -- if it fails, record the conflict.
                 (with-handlers
                   ([exn:fail:filesystem?
                      (lambda ([exn: exn])
                        (cons (conflict type project file contents 'missing-directory (if history (string->bytes/utf-8 history) #"")) conflicts))])
                   (parameterize ([current-directory (build-project-path project)])
                     (make-directory* base))
                   ;; Everything OK, write file
                   (attempt-to-write-file))
                 ;; Just write file.
                 (attempt-to-write-file))]
               [else
                 (raise (exn:project:sync "Expected non-#f contents for editFile!"
                                          (current-continuation-marks)))]]])))
    '()
    their-changes))

(: resolve-conflicts (-> Bytes (Listof Conflict-Type) (Listof off:conflict)))
(define (resolve-conflicts timestamp conflicts)
  (for/list : (Listof off:conflict)
    ([cft : Conflict-Type conflicts])
    (match-define (conflict type project file contents reason history) cft)
    (with-handlers
      ;; Ignore errors when handling conflicts,
      ;; but record that resolving the conflict failed.
      ([exn:fail? (lambda ([exn : exn])
                    (off:conflict type (off:file project file #f)
                                  (format "Exception occurred while handling conflict: ~a.  Origional reason: ~a"
                                          (exn-message exn) (reason->string reason))
                                  #f))])
      ;; Deal with type of conflict
      (cond
        ;; editFile - write out file contents.
        [(or (equal? type "newFile") (equal? type "editFile"))
          ;; Generate new extension.
          (define fext (filename-extension file))
          (define newext
            (bytes-append #"_conflict_" timestamp (if fext (bytes-append #"." fext) #"")))
          ;; Calculate location for conflict file.
          (define-values (base rel-file _2) (split-path (check-path (build-path file))))
          (cond
            [(path? rel-file)
              (: calculate-conflict-location (-> (U False 'relative Path-String) Path-String))
              (define (calculate-conflict-location base)
                (cond
                  [(equal? base 'relative) rel-file]
                  [(not base) rel-file]
                  [(directory-exists? (build-path (build-project-path project) base))
                   (build-path base rel-file)]
                  [else
                    (define-values (new-base _1 _2) (split-path base))
                    (calculate-conflict-location new-base)]))
              (define file-to-write (path-replace-suffix (calculate-conflict-location base) newext))
              (when contents
                ;; This call to new-file should not fail.
                (new-file project file-to-write (string->bytes/utf-8 contents) 'raw #f))
              (off:conflict type (off:file project file #f) (reason->string reason) #t)]
            [else (raise (exn:project:sync "Path did not refer to file!" (current-continuation-marks)))])]
        [else
          (off:conflict type (off:file project file #f) (reason->string reason) #t)]))))

(: fetch-files-for (-> String (Listof off:file)))
(define (fetch-files-for project)
  (foldl
    (lambda ([file : (List String Boolean Number (U False String))]
             [result : (Listof off:file)])
      (match-define (list path is-dir? mtime checksum) file)
      (if is-dir?
        result
        (cons  (off:file project path (begin (assert checksum) checksum)) result)))
    '()
    (list-files project)))

(: fetch-all-files (-> (Listof off:file)))
(define (fetch-all-files)
  (apply append
         (map
           fetch-files-for
           (map
             (lambda ([rst : (List String Number)]) : String
              (first rst))
            (list-projects)))))

(: strip-checksum (-> off:file off:file))
(define (strip-checksum f)
  (match f
    [(off:file project file _) (off:file project file #f)]))

(: list-off-projects (-> (Listof off:project)))
(define (list-off-projects)
  (map (lambda ([p : (List String Integer)])
        (off:project (first p) (second p)
          (jsexpr->string (cast (read-project-settings (car p)) JSExpr))))
       (list-projects)))

(: sanitize-settings (-> (HashTable Symbol Any) Settings))
(define (sanitize-settings hsh)
  (make-hasheq
    (cast (filter (lambda ([pair : (Pairof Symbol Any)]) (or (string? (cdr pair)) (number? (cdr pair))))
            (hash->list hsh)) (Listof (Pairof Symbol (U String Number))))))

(: empty-settings (-> Settings))
(define (empty-settings)
  (hasheq))

(: sync-offline-changes (-> JSExpr JSExpr))
(define (sync-offline-changes js-changeset)
  ;; TODO: Grab global lock to protect against _all_ operations.
  ;; Do not want other operations modifying state that the offline engine needs to work with.

  ;; Back up the entire projects directory
  (define temp-projects (build-path (read-config-path 'seashell)
                          (string-append "projects-" (number->string (current-seconds)))))
  (copy-directory/files (project-base-path) temp-projects)

  (define sync-result (with-handlers
    ([exn:fail? (lambda ([err : exn])
      (delete-directory/files (project-base-path))
      (rename-file-or-directory temp-projects (project-base-path))
      (raise err))])
    (define timestamp (make-timestamp))
    (define changeset (json->off:changes js-changeset))
    (match-define
      (off:changes their-projects their-files their-changes their-settings)
      changeset)
    ;; NOTE: We do not expect new projects, hence it is safe to apply the changes first
    ;; before looking at projects/files.

    ;; If nontrivial changes exist, record the changeset in case something goes wrong.
    ;; TODO: Write scripts so instructional support staff can replay/fix changesets
    ;; that go bad.
    (when (not (empty? their-changes))
      (record-offline-changeset timestamp js-changeset))

    ;; Apply changes, collect conflicts.
    (define conflicts (apply-offline-changes their-changes))

    ;; Sync global settings
    (define-values (our-settings our-settings-modified) (read-settings))
    (logf 'debug "local mod: ~a" (off:settings-modified their-settings))
    (logf 'debug "serve mod: ~a" our-settings-modified)
    (define result-settings
      (if (and our-settings (< (off:settings-modified their-settings) our-settings-modified))
          (jsexpr->string (cast our-settings JSExpr))
          #f))
    (unless result-settings
      (write-settings (string->jsexpr (off:settings-values their-settings))))

    ;; Collect list of new projects.
    (define our-projects (list-off-projects))
    (define our-projects-name (map (lambda ([l : off:project]) (off:project-name l))
                                our-projects))
    (define their-projects-name (map off:project-name their-projects))
    (define deleted-projects (remove* our-projects-name their-projects-name))
    (define new-projects (remove* their-projects-name our-projects-name))

    ;; Overwrite project settings that are newer in the offline storage
    (define updated-projects
      (map (lambda ([l : (Listof off:project)]) (first l))
        (filter (lambda ([l : (Listof off:project)])
                  (and (= (length l) 2) (> (off:project-last_modified (first l))
                                           (off:project-last_modified (second l)))))
          (map (lambda ([p : off:project]) (cons p
              (filter (lambda ([pp : off:project]) (string=? (off:project-name pp) (off:project-name p)))
                      their-projects)))
            our-projects))))

    (define updated-projects-name (map off:project-name updated-projects))
    (define offline-updated-projects
      (filter (lambda ([p : off:project])
                (and (member (off:project-name p) our-projects)
                     (not (member (off:project-name p) updated-projects-name))))
              their-projects))

    ;; update projects that were modified locally while offline
    (map (lambda ([p : off:project])
           (write-project-settings (off:project-name p)
                ((lambda ([p : off:project]) (if (off:project-settings p)
                    (sanitize-settings (cast (string->jsexpr (off:project-settings p))
                                         (HashTable Symbol Any)))
                    (empty-settings))) p)))
         offline-updated-projects)

    ;; Resolve conflicts (add .conflict for each file)
    (define conflict-information (resolve-conflicts timestamp conflicts))

    ;; After this point the changeset has been committed to disk.
    ;; Collect list of files in the backend.
    (define our-files (fetch-all-files))
    (define our-files/w-c (map strip-checksum our-files))
    (define their-files/w-c (map strip-checksum their-files))
    ;; Collect list of deleted files (in the backend).
    (define backend-new-files (list->set (remove* their-files/w-c our-files/w-c)))
    (define backend-deleted-files (remove* our-files/w-c their-files/w-c))
    (define our-delete-change
      (map (lambda ([f : off:file]) : off:change (off:change "deleteFile" f #f #f #f))
           backend-deleted-files))
    ;; Collect list of new/edited files (in the backend).
    ;; NOTE: The checksum matters for this calculation.
    (define backend-changed-files (remove* their-files our-files))
    ;; NOTE: Binary files are ignored when sending back list of new files.
    ;; TODO: Properly handle binary files (as base64 data: URLs).
    (define our-edit-changes
      (foldl
        (lambda ([change : (U False off:change)]
                 [changes : (Listof off:change)])
          (if change (cons change changes) changes))
        '()
        (map (lambda ([f : off:file]) : (U False off:change)
               (define-values (contents checksum history)
                 (read-file (off:file-project f) (off:file-file f)))
               (define is-new-file? (set-member? backend-new-files (strip-checksum f)))
               (with-handlers
                 ;; Send a placeholder record for binary files (that are not available offline).
                 ([exn:fail? (lambda ([exn : exn])
                               (off:change (if is-new-file? "newFile" "editFile")
                                           f #f
                                           (if is-new-file? #f checksum)
                                           (if is-new-file? #f (bytes->string/utf-8 history))))])
                 (off:change (if is-new-file? "newFile" "editFile")
                             f (bytes->string/utf-8 contents)
                             (if is-new-file? #f checksum)
                             (if is-new-file? #f (bytes->string/utf-8 history)))))
             backend-changed-files)))
    ;; Generate changes to send back.
    (off:response->json
        (off:response
          conflict-information
          (append our-delete-change our-edit-changes)
          new-projects
          deleted-projects
          updated-projects
          result-settings))))

  (delete-directory/files temp-projects)
  sync-result)
