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
(require "support.rkt")
(require "updates.rkt")

(provide (struct-out database-change)
         row->change
         reduce-changes
         resolve-conflicts)

(struct database-change ([type : Integer] [client : String] [table : String] [key : String] [data : String]) #:transparent)

(: true? (All (A) (-> (Option A) Any : #:+ A)))
(define (true? x) x)

;; (row-change SQLRow) -> database-change
;; Converts a row in the SQLite3 database to a database-change struct.
;; Arguments:
;;  row - Row as given by query-row.
(: row->change (-> (Vectorof SQL-Datum) database-change))
(define (row->change row)
  (define type (assert (vector-ref row 0) exact-integer?))
  (define client (assert (vector-ref row 1) string?))
  (define table (assert (vector-ref row 2) string?))
  (define key (assert (vector-ref row 3) string?))
  (define data (assert (vector-ref row 4) string?))
  (database-change type client table key data))

;; (reduce-changes changes) -> (HashTable (table key) -> change)
;; Given a list of database changes, returns a hash table that
;; maps each (table, primary key) pair to the change that is the result
;; of applying all the database changes provided.
;;
;; Arguments:
;;  changes - List of database changes (as database-change)
(: reduce-changes (-> (Listof database-change) (HashTable (Pair String String) database-change)))
(define (reduce-changes changes)
  (for/fold ([changes : (HashTable (Pair String String) database-change) (hash)])
            ([change : database-change changes])
    (match-define (database-change type client table key data) change)
    (define change-key (cons table key))
    (cond
      [(hash-has-key? changes change-key)
       (define old-change (hash-ref changes change-key))
       (match-define (database-change old-type old-client old-table old-key old-data) old-change)
       (hash-set changes change-key
                 (cond
                   [(= old-type CREATE)
                    (cond
                      [(= type UPDATE) (database-change CREATE SERVER_MERGE_KEY table key (fold-create-and-update old-data data))]
                      [else change])]
                   [(= old-type UPDATE)
                    (cond
                      [(= type UPDATE) (database-change UPDATE SERVER_MERGE_KEY table key (fold-update-and-update old-data data))]
                      [else change])]
                   [(= old-type DELETE)
                    (cond
                      [(= type CREATE) change]
                      [else old-change])]
                   [else change]))]
      [else (hash-set changes change-key change)])))

;; (resolve-conflicts client-changes server-changes) -> reconciled-changes
;; Given a list of client changes and server changes, return a list of reconciled changes
;; that will represent the changes that need to be applied to the client/server in order
;; to have the database in-sync with both ends.
;;
;; Arguments:
;;  client-changes - List of client changes.
;;  server-changes - List of server changes.
(: resolve-conflicts (-> (Listof database-change) (Listof database-change) (Listof database-change)))
(define (resolve-conflicts client-changes server-changes)
  (define reduced-changes (reduce-changes server-changes))
  (filter (inst true? database-change)
          (for/list : (Listof (Option database-change))
            ([change : database-change client-changes])
            (match-define (database-change client-type client client-table client-key client-data) change)
            (define client-change-id (cons client-table client-key))
            (cond
              [(hash-has-key? reduced-changes client-change-id)
               (define server-change (hash-ref reduced-changes client-change-id))
               (define server-type (database-change-type server-change))
               (define server-data (database-change-data server-change))
               (cond
                 [(= UPDATE server-type)
                  (cond
                    [(= CREATE client-type)
                     (database-change client-type client client-table client-key
                                      (apply-modifications client-data server-data))]
                    [(= DELETE client-type)
                     change]
                    [(= UPDATE client-type)
                     (define parsed-client-mods (assert (string->jsexpr client-data) hash?))
                     (define parsed-server-mods (assert (string->jsexpr server-data) hash?))
                     (define result (for/fold
                                     ([new-client-data : (HashTable Symbol JSExpr) parsed-client-mods])
                                     ([(_server-key _) (in-hash parsed-server-mods)])
                                      (define server-key (symbol->string _server-key))
                                      (for/fold
                                       ([temp-client-data : (HashTable Symbol JSExpr) (hash-remove new-client-data _server-key)])
                                       ([(_client-key _2) (in-hash parsed-client-mods)])
                                        (define client-key (symbol->string _client-key))
                                        (if (string-prefix? client-key (string-append server-key "."))
                                            (hash-remove temp-client-data _client-key)
                                            temp-client-data))))
                     (if (not (hash-empty? result))
                         (database-change client-type client client-table client-key
                                          (jsexpr->string result))
                         #f)]
                    [else #f])]
                 [else #f])]
              [else change]))))
