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

(require "database.rkt")

(provide init-tables
         select-files-for-project
         delete-files-for-project
         filename-exists?)

;; This file contains various Seashell-specific database queries beyond the basic
;; Dexie syncable interface. These will be used by the backend support tools as well as the
;; Seashell server when doing things like cloning skeleton projects.

(: seashell-database (U (Instance Sync-Database%) False))
(define seashell-database #f)

(: get-database (-> (Instance Sync-Database%)))
(define (get-database)
  (unless seashell-database
    (set! seashell-database (make-object sync-database% 'memory)))
  (cast seashell-database (Instance Sync-Database%)))

;; Probably will only use this in the tests
(: init-tables (-> Void))
(define (init-tables)
  (define db (get-database))
  (send db write-transaction (thunk
    (query-exec (send db get-conn) "CREATE TABLE IF NOT EXISTS projects (id TEXT PRIMARY KEY, data TEXT)")
    (query-exec (send db get-conn) "CREATE TABLE IF NOT EXISTS files (id TEXT PRIMARY KEY, data TEXT)")
    (query-exec (send db get-conn) "CREATE TABLE IF NOT EXISTS contents (id TEXT PRIMARY KEY, data TEXT)"))))

(: select-files-for-project (-> String (Listof JSExpr)))
(define (select-files-for-project pid)
  (define db (get-database))
  (define result
    (query-rows (send db get-conn)
      "SELECT json(data) FROM files WHERE json_extract(data,'$.project_id')=$1" pid))
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
