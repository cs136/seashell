#lang typed/racket
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
(require seashell/db/changes
         seashell/db/support
         seashell/db/database
         seashell/websocket
         seashell/log
         (submod seashell/seashell-config typed)
         typed/json)

(provide sync-server%
         Sync-Server%)

(define-type Sync-Server% (Class
  (init [connect Websocket-Connection])
  [client-identity (-> (U String False) String)]
  [create-database-change (-> JSExpr database-change)]
  [subscribe (-> (U Integer False) Void)]
  [sync-changes (-> (Listof database-change) Integer Boolean Void)]))

;; (new sync-server connection)
;; Sync server interface class
;; Provides helper functions to interface with the database.
(: sync-server% : Sync-Server%)
(define sync-server%
  (class object%
    (init [connect : Websocket-Connection])

    (: conn Websocket-Connection)
    (define conn connect)
    (super-new)

    ;; current-client --> UUID of the current client, #f if not ready.
    (: current-client (U False String))
    (define current-client #f)

    ;; synced-revision --> last known revision sent to the client, 0 if unknown.
    (: synced-revision Integer)
    (define synced-revision 0)

    ;; database --> Connection to the global sync database.
    (: database (Instance Sync-Database%))
    (define database (get-sync-database))

    ;; send-message (-> JSExpr Void)
    ;; Helper function for sending a JSON message across the websocket connection.
    (: send-message (-> JSExpr Void))
    (define/private (send-message msg)
      (ws-send conn (jsexpr->bytes msg)))

    ;; client-identity (-> Option String) String)
    ;; Sets or updates the current client identity.
    (: client-identity (-> (U String False) String))
    (define/public (client-identity identity)
      (cond
        [identity
          (set! current-client identity)
          identity]
        [else
          (set! current-client (send database create-client))
          (assert current-client)]))

    ;; get-send-changes (-> (-> Void))
    ;; Creates a thunk that will fetch any new changes from the database and send them
    ;; to the client.
    ;;
    ;; Will unregister client if the connection is dead.
    (: get-send-changes (-> (-> Void)))
    (define/private (get-send-changes)
      (lambda ()
        (cond
            [(not (ws-connection-closed? conn))
             (send database read-transaction (thunk
               (define changes (map (lambda ([chg : (Pairof (Pairof String String) database-change)])
                   (database-change->json (cdr chg)))
                 (hash->list (reduce-changes (map row->change
                   (send database fetch-changes synced-revision (assert current-client)))))))
               (define rev (send database current-revision))
               (send-message
                 #{`#hasheq((id . -5)
                            (success . #t)
                            (result . 
                              ,#{`#hasheq((type . "changes")
                                      (changes . ,changes)
                                      (currentRevision . ,rev)
                                      (partial . #f)) :: JSExpr})) :: JSExpr})
               (set! synced-revision rev)))]
            [else
             (logf 'info "Client ~a dead, unsubscribing." current-client)
             (when current-client
              (send database unsubscribe (assert current-client)))])))



    ;; create-database-change (-> JSExpr database-change)
    ;; Given a JSExpr representing the change from the frontend,
    ;; tags it with the current client id and converts it to a database-change object.
    (: create-database-change (-> JSExpr database-change))
    (define/public (create-database-change chg)
      (define change (cast chg (HashTable Symbol JSExpr)))
      (json->database-change (if (hash-has-key? change 'client)
        change
        (hash-set change 'client (assert current-client)))))

    ;; subscribe (-> (Option Integer) Void)
    ;; Subscribes to updates from the database, listening to all updates after revision.
    (: subscribe (-> (U Integer False) Void))
    (define/public (subscribe revision)
      (set! synced-revision (if revision revision 0))
      (send database read-transaction (thunk
        ((get-send-changes))
        (when current-client
          (send database subscribe (assert current-client) (get-send-changes))))))

    ;; sync-changes (-> (Listof database-change) Integer Boolean Void))
    ;; Updates the database.
    ;;
    ;; Arguments:
    ;;   changes - List of changes to apply.
    ;;   revision - Revision for which these changes apply to.
    ;;   partial - #t if only a partial set of changes, #f otherwise.
    (: sync-changes (-> (Listof database-change) Integer Boolean Void))
    (define/public (sync-changes changes revision partial)
      (send database write-transaction (thunk
        (cond
          [partial
            (map (lambda ([chg : database-change])
              (cond
                [(= (database-change-type chg) CREATE)
                  (send database apply-partial-create
                    (database-change-table chg)
                    (database-change-key chg)
                    (cast (string->jsexpr (database-change-data chg)) DBExpr)
                    (database-change-client chg))]
                [(= (database-change-type chg) UPDATE)
                  (send database apply-partial-update
                    (database-change-table chg)
                    (database-change-key chg)
                    (cast (string->jsexpr (database-change-data chg)) DBExpr)
                    (database-change-client chg))]
                [(= (database-change-type chg) DELETE)
                  (send database apply-partial-delete
                    (database-change-table chg)
                    (database-change-key chg)
                    (database-change-client chg))]))
              changes)
            (void)]
          [else
            (define base (if revision revision 0))
            (define srv-changes (map row->change (send database fetch-changes base (assert current-client))))
            (define resolved (resolve-conflicts changes srv-changes))
            (map (lambda ([chg : database-change])
              (cond
                [(= (database-change-type chg) CREATE)
                  (send database apply-create
                    (database-change-table chg)
                    (database-change-key chg)
                    (cast (string->jsexpr (database-change-data chg)) DBExpr)
                    (database-change-client chg))]
                [(= (database-change-type chg) UPDATE)
                  (send database apply-update
                    (database-change-table chg)
                    (database-change-key chg)
                    (cast (string->jsexpr (database-change-data chg)) DBExpr)
                    (database-change-client chg))]
                [(= (database-change-type chg) DELETE)
                  (send database apply-delete
                    (database-change-table chg)
                    (database-change-key chg)
                    (database-change-client chg))]))
              resolved)
            (void)]))))))
