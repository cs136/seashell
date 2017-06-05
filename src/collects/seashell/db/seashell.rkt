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

(require typed/json)
(require typed/db)

(require/typed racket/random
  [crypto-random-bytes (-> Integer Bytes)])

(require seashell/db/database)

(provide get-uuid
         init-database
         insert-new
         select-id
         delete-id
         select-projects
         select-project-name
         select-files-for-project
         delete-files-for-project
         filename-exists?
         call-with-read-transaction
         call-with-write-transaction)

;; This file contains various Seashell-specific database queries beyond the basic
;; Dexie syncable interface. These will be used by the backend support tools as well as the
;; Seashell server when doing things like cloning skeleton projects.

(: seashell-database (U (Instance Sync-Database%) False))
(define seashell-database #f)

(: get-database (-> (Instance Sync-Database%)))
(define (get-database)
  (unless seashell-database
    (error "Must call init-database before accessing the database."))
  (cast seashell-database (Instance Sync-Database%)))

(: get-uuid (-> String))
(define (get-uuid)
  ;; TODO change this to actual UUIDs
  (bytes->string/utf-8 (crypto-random-bytes 16) (integer->char (random 256))))

;; Must be called before accessing the database
(: init-database (->* () (SQLite3-Database-Storage) Void))
(define (init-database [storage 'memory])
  (set! seashell-database (make-object sync-database% storage))
  (init-tables))

;; Probably will only use this in the tests and the first time Seashell is run for a user
(: init-tables (-> Void))
(define (init-tables)
  (define db (get-database))
  (send db write-transaction (thunk
    (query-exec (send db get-conn) "CREATE TABLE IF NOT EXISTS projects (id TEXT PRIMARY KEY, data TEXT)")
    (query-exec (send db get-conn) "CREATE TABLE IF NOT EXISTS files (id TEXT PRIMARY KEY, data TEXT)")
    (query-exec (send db get-conn) "CREATE TABLE IF NOT EXISTS contents (id TEXT PRIMARY KEY, data TEXT)"))))

;; Inserts the given object, generating a new UUID for it
(: insert-new (->* (String DBExpr) (String) String))
(define (insert-new table object [id #f])
  (define new-id (if id id (get-uuid)))
  (send (get-database) apply-create table new-id object)
  new-id)

(: select-id (-> String String (Option JSExpr)))
(define (select-id table id)
  (send (get-database) fetch table id))

(: delete-id (-> String String Void))
(define (delete-id table id)
  (void (send (get-database) apply-delete table id)))

(: select-projects (-> (Listof JSExpr)))
(define (select-projects)
  (define db (get-database))
  (define result
    (query-rows (send db get-conn)
      "SELECT json_insert(data, '$.id', id) FROM projects"))
  (map (lambda ([x : (Vectorof SQL-Datum)]) (string->jsexpr (cast (vector-ref x 0) String))) result))

;; TODO: handle the case where a project with that name does not exist
(: select-project-name (-> String JSExpr))
(define (select-project-name name)
  (define db (get-database))
  (string->jsexpr (cast (vector-ref (first (query-rows (send db get-conn)
    "SELECT json_insert(data, '$.id', id) FROM projects WHERE json_extract(data, '$.name')=$1" name)) 0) String)))

(: select-files-for-project (-> String (Listof JSExpr)))
(define (select-files-for-project pid)
  (define db (get-database))
  (define result
    (query-rows (send db get-conn)
      "SELECT json_insert(data, '$.id', id) FROM files WHERE json_extract(data,'$.project_id')=$1" pid))
  (map (lambda ([x : (Vectorof SQL-Datum)]) (string->jsexpr (cast (vector-ref x 0) String))) result))

(: delete-files-for-project (-> String Void))
(define (delete-files-for-project pid)
  (define db (get-database))
  (send db write-transaction (thunk
    (define files (select-files-for-project pid))
    (void (map (lambda ([x : JSExpr])
      (send db apply-delete "files" (cast (hash-ref (cast x (HashTable Symbol JSExpr)) 'id) String)))
      files)))))

(: filename-exists? (-> String String Boolean))
(define (filename-exists? pid name)
  (define db (get-database))
  (not (false?
    (query-maybe-value (send db get-conn)
      "SELECT json(data) FROM files WHERE json_extract(data, '$.project_id')=$1 AND json_extract(data, '$.name')=$2"
      pid name))))

(: call-with-read-transaction (All (A) (-> (-> A) A)))
(define (call-with-read-transaction tnk)
  (send (get-database) read-transaction tnk))

(: call-with-write-transaction (All (A) (-> (-> A) A)))
(define (call-with-write-transaction tnk)
  (send (get-database) write-transaction tnk))
