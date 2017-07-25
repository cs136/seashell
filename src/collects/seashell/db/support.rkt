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
(require typed/db/sqlite3)

(provide string-or-jsexpr->string
         jsexpr-or-string->jsexpr
         set-by-key-path
         delete-by-key-path
         sqlite-connection
         compute-conn
         CREATE UPDATE DELETE
         SERVER_CLIENT_KEY
         SERVER_MERGE_KEY
         db-in-transaction?)

(define CREATE 1)
(define UPDATE 2)
(define DELETE 3)
(define SERVER_CLIENT_KEY "__SERVER_CHANGE")
(define SERVER_MERGE_KEY "__SERVER_MERGED_CHANGE")
(: db-in-transaction? (Parameter Boolean))
(define db-in-transaction? (make-parameter #f))

;; (sqlite-connection path) -> Connection
;; Opens a virtual connection to the specified SQLite3 database.
;; Arguments:
;;  path: Either a string or 'memory
;; Returns:
;;  A connection to the wanted SQLite3 database.
(: sqlite-connection (-> SQLite3-Database-Storage Connection))
(define (sqlite-connection path)
  (virtual-connection
   (connection-pool
    (thunk 
      (if (or (not (path-string? path)) (file-exists? path))
          (sqlite3-connect #:database path #:use-place #f)
          (sqlite3-connect #:database path #:use-place #f #:mode 'create))))))
(define compute-conn (sqlite-connection 'memory))

;; (string-or-jsexpr->string expr) -> String
;; Converts a JSON expression or a String to a String.
;;
;; Arguments:
;;  expr -> JSExpr | String
(: string-or-jsexpr->string (-> (U String JSExpr) String))
(define (string-or-jsexpr->string jsexpr)
  (if (string? jsexpr) jsexpr (jsexpr->string jsexpr)))

;; (string-or-jsexpr->string expr) -> String
;; Converts a string or a JSON expression to a JSON expression.
;;
;; Arguments:
;;  expr -> JSExpr | String
(: jsexpr-or-string->jsexpr (-> (U String JSExpr) JSExpr))
(define (jsexpr-or-string->jsexpr x)
  (if (string? x) (string->jsexpr x) x))

;; (set-by-key-path object key update) -> newObject (as String)
;; Sets the specified key in object to the new value (specified in update)
;; Returns a stringified representation of the resulting object.
;;
;; Arguments:
;;  object - JSExpr | String representing the object to update.
;;  key    - Key path to update.
;;  update - New value to set.
(: set-by-key-path (-> (U String JSExpr) String String String))
(define (set-by-key-path object key update)
  (assert (query-value compute-conn "SELECT json_set(json($1), $2, json($3))"
                       (string-or-jsexpr->string object)
                       (string-append "$." key)
                       update) string?))

;; (delete-by-key-path object key update) -> newObject (as String)
;; Removes the specified key in object to the new value (specified in update)
;; Returns a stringified representation of the resulting object.
;;
;; Arguments:
;;  object - JSExpr | String representing the object to update.
;;  key    - Key path to delete..
(: delete-by-key-path (-> (U String JSExpr) String String))
(define (delete-by-key-path object key)
  (assert (query-value compute-conn "SELECT json_remove(json($1), $2)"
                       (string-or-jsexpr->string object)
                       (string-append "$." key)) string?))
