#lang typed/racket
;; Seashell's SQLite3 + Dexie bindings.
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

(struct exn:database exn:fail:user ())

(require typed/json
         typed/db
         typed/db/sqlite3
         (submod seashell/seashell-config typed)
         seashell/log
         seashell/utils/uuid
         seashell/db/database)

(provide insert-new
         select-id
         delete-id
         select-projects
         select-project-name
         select-files-for-project
         delete-files-for-project
         delete-everything
         backup-database
         filename-exists?
         project-exists?
         seashell-collect-garbage
         call-with-read-transaction
         call-with-write-transaction)

;; This file contains various Seashell-specific database queries beyond the basic
;; Dexie syncable interface. These will be used by the backend support tools as well as the
;; Seashell server when doing things like cloning skeleton projects.

;; Inserts the given object, generating a new UUID for it
(: insert-new (->* (String DBExpr) (String) String))
(define (insert-new table object [id #f])
  (define new-id (if id id (uuid-generate)))
  (send (get-sync-database) apply-create table new-id object)
  new-id)

(: select-id (-> String String (Option JSExpr)))
(define (select-id table id)
  (send (get-sync-database) fetch table id))

(: delete-id (-> String String Void))
(define (delete-id table id)
  (void (send (get-sync-database) apply-delete table id)))

(: select-projects (-> (Listof JSExpr)))
(define (select-projects)
  (define db (get-sync-database))
  (define result
    (query-rows (send db get-conn)
      "SELECT json_insert(data, '$.id', id) FROM projects"))
  (map (lambda ([x : (Vectorof SQL-Datum)]) (string->jsexpr (cast (vector-ref x 0) String))) result))

;; TODO: handle the case where a project with that name does not exist
(: select-project-name (-> String JSExpr))
(define (select-project-name name)
  (define db (get-sync-database))
  (string->jsexpr (cast (vector-ref (first (query-rows (send db get-conn)
    "SELECT json_insert(data, '$.id', id) FROM projects WHERE json_extract(data, '$.name')=$1" name)) 0) String)))

(: select-files-for-project (-> String (Listof JSExpr)))
(define (select-files-for-project pid)
  (define db (get-sync-database))
  (define result
    (query-rows (send db get-conn)
      "SELECT json_insert(data, '$.id', id) FROM files WHERE json_extract(data,'$.project_id')=$1" pid))
  (map (lambda ([x : (Vectorof SQL-Datum)]) (string->jsexpr (cast (vector-ref x 0) String))) result))

(: delete-files-for-project (-> String Void))
(define (delete-files-for-project pid)
  (define db (get-sync-database))
  (send db write-transaction (thunk
    (define files (select-files-for-project pid))
    (void (map (lambda ([x : JSExpr])
      (send db apply-delete "files" (cast (hash-ref (cast x (HashTable Symbol JSExpr)) 'id) String)))
      files)))))

(: delete-everything (-> Void))
(define (delete-everything)
  (define db (get-sync-database))
  (send db write-transaction (thunk
    ;; table by table, apply each delete one by one
    (define projects (query-rows (send db get-conn)
      "SELECT id FROM projects"))
    (for-each (lambda ([x : (Vectorof SQL-Datum)])
      (send db apply-delete "projects" (cast (vector-ref x 0) String)))
      projects)
    (define files (query-rows (send db get-conn)
      "SELECT id FROM files"))
    (for-each (lambda ([x : (Vectorof SQL-Datum)])
      (send db apply-delete "files" (cast (vector-ref x 0) String)))
      files)
    (define contents (query-rows (send db get-conn)
      "SELECT id FROM contents"))
    (for-each (lambda ([x : (Vectorof SQL-Datum)])
      (send db apply-delete "contents" (cast (vector-ref x 0) String)))
      contents))))

(: backup-database (-> String Void))
(define (backup-database target)
  (define db-path (send (get-sync-database) get-path))
  (unless (or (path? db-path) (string? db-path))
    (raise (exn:database "Cannot backup a database which is not stored in the filesystem."
      (current-continuation-marks))))
  (define-values (proc out in err)
    (subprocess #f #f #f (read-config-string 'sqlite3) (cast db-path Path-String)
      (format ".backup ~a" target)))
  (subprocess-wait proc)
  (close-output-port in)
  (close-input-port out)
  (close-input-port err)
  (define exit-status (cast (subprocess-status proc) Integer))
  (unless (zero? exit-status)
    (raise (exn:database "Failed to backup database." (current-continuation-marks)))))

(: project-exists? (-> String Boolean))
(define (project-exists? proj)
  (define db (get-sync-database))
  (not (false?
    (query-maybe-value (send db get-conn)
      "SELECT json(data) FROM projects WHERE json_extract(data, '$.name')=$1"
      proj))))

(: filename-exists? (-> String String Boolean))
(define (filename-exists? pid name)
  (define db (get-sync-database))
  (not (false?
    (query-maybe-value (send db get-conn)
      "SELECT json(data) FROM files WHERE json_extract(data, '$.project_id')=$1 AND json_extract(data, '$.name')=$2"
      pid name))))

;; This function should be run periodically to clear out old contents entries that will clog
;;  up the database.
(: seashell-collect-garbage (-> Void))
(define (seashell-collect-garbage)
  (logf 'info "Beginning garbage collection.")
  (define db (get-sync-database))
  ;; get all contents that are not currently in use
  (call-with-write-transaction (thunk
    (define data (query-rows (send db get-conn)
      "SELECT id, json(data) FROM contents WHERE id NOT IN
        (SELECT json_extract(data, '$.contents_id') FROM files)"))

    ;; get files out of query and group them by project and filename
    (: group-files (-> (Listof (Vectorof SQL-Datum)) (Listof (Listof DBExpr))))
    (define (group-files res)
      (group-by (lambda ([f : DBExpr])
        (cons (cast (hash-ref f 'project_id) String)
              (cast (hash-ref f 'filename) String)))
        (map (lambda ([x : (Vectorof SQL-Datum)])
          (hash-set
            (cast (string->jsexpr (cast (vector-ref x 1) String)) DBExpr)
            'id (cast (vector-ref x 0) JSExpr)))
             res)
        (lambda ([a : (Pairof String String)] [b : (Pairof String String)])
          (and (string=? (car a) (car b)) (string=? (cdr a) (cdr b))))))

    ;; millisecond quantities
    (define hour (* 1000 60 60))
    (define day (* hour 24))
    (define week (* day 7))
    (define month (* day 30))
    ;; number of extra entries to keep from each time division.
    ;; everything is kept from the last hour
    (define day-quota 25)
    (define week-quota 25)
    (define month-quota 25)
    ;; returns content ids that should be deleted based on the given time
    (: get-expendable-contents (-> (Listof (Pairof String Integer)) Integer (Listof String)))
    (define (get-expendable-contents conts time)
      (define diffs (map (lambda ([c : (Pairof String Integer)])
        (cons (car c) (- time (cdr c))))
        conts))
      (define hour-len (count (lambda ([c : (Pairof String Integer)])
        (<= (cdr c) hour))
        diffs))
      (define today (filter (lambda ([c : (Pairof String Integer)])
        (<= (cdr c) day))
        (drop diffs hour-len)))
      (define today-len (length today))
      (define this-week (filter (lambda ([c : (Pairof String Integer)])
        (<= (cdr c) week))
        (drop diffs (+ hour-len today-len))))
      (define this-week-len (length this-week))
      (define this-month (filter (lambda ([c : (Pairof String Integer)])
        (<= (cdr c) month))
        (drop diffs (+ hour-len today-len this-week-len))))
      (define this-month-len (length this-month))
      (define older (drop diffs (+ hour-len today-len this-week-len this-month-len)))

      ;; This will select the "dense" contents entries that we want to delete.
      ;; For now we evenly space them out in terms of their position in
      ;;  the list of most recent (not in terms of position in time).
      (: select-dense-elements (-> (Listof (Pairof String Integer)) Integer (Listof (Pairof String Integer))))
      (define (select-dense-elements lst target-len)
        (define len (length lst))
        (define sep (quotient len target-len))
        (if (> len target-len)
          (cdr (foldl (lambda ([c : (Pairof String Integer)]
                          [res : (Pairof Integer (Listof (Pairof String Integer)))])
            #{(cons (+ 1 (car res))
                        (if (= (remainder (car res) sep) 0)
                            (cdr res)
                            (cons c (cdr res))))
                  :: (Pairof Integer (Listof (Pairof String Integer)))})
            #{(cons 0 '()) :: (Pairof Integer (Listof (Pairof String Integer)))} lst))
          '()))

      (map #{car :: (-> (Pairof String Integer) String)} (append
        (select-dense-elements today day-quota)
        (select-dense-elements this-week week-quota)
        (select-dense-elements this-month month-quota)
        older)))

    ;; just grab content ids, timestamps, and sort by timestamp
    (define ordered-files
      (map (lambda ([clst : (Listof DBExpr)])
          (sort (map (lambda ([c : DBExpr])
            (cons (cast (hash-ref c 'id) String)
                  (cast (hash-ref c 'time) Integer)))
            clst) (lambda ([a : (Pairof String Integer)] [b : (Pairof String Integer)])
              (< (cdr a) (cdr b)))))
        (group-files data)))

    (define now (current-milliseconds))
    (define num-deleted 0)
    (for-each (lambda ([f : (Listof (Pairof String Integer))])
      (define to-delete (get-expendable-contents f now))
      (for-each (lambda ([cid : String])
        (set! num-deleted (+ 1 num-deleted))
        (send db apply-delete "contents" cid))
        to-delete))
      ordered-files)
    (logf 'info "Garbage collection completed, deleted ~a contents entries." num-deleted))))

(: call-with-read-transaction (All (A) (-> (-> A) A)))
(define (call-with-read-transaction tnk)
  (send (get-sync-database) read-transaction tnk))

(: call-with-write-transaction (All (A) (-> (-> A) A)))
(define (call-with-write-transaction tnk)
  (send (get-sync-database) write-transaction tnk))
