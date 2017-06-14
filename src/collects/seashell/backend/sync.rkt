#lang typed/racket

(require seashell/db/changes
         seashell/db/database
         seashell/websocket
         seashell/config
         typed/json)

(provide sync-server%
         Sync-Server%)

(define-type Sync-Server% (Class
  (init [conn : Websocket-Connection])
  [client-identity (-> (U String False) String)]
  [subscribe (-> (U Integer False) Void)]
  [sync-changes (-> (Listof database-change) Integer Boolean Void)]))

(: sync-server% : Sync-Server%)
(define sync-server%
  (class object%
    (init [conn : Websocket-Connection])

    (super-new)

    (: current-client (U False String))
    (define current-client #f)

    (: synced-revision Integer)
    (define synced-revision 0)

    (: database Sync-Database%)
    (define database (make-object sync-database%
      (build-path (read-config-path 'seashell) (read-config-path 'database-file))))

    (thread (thunk
      (sync (ws-connection-closed-evt conn))
      (send database unsubscribe send-changes)))

    (: send-message (-> JSExpr Void))
    (define/private (send-message msg)
      (ws-send conn (jsexpr->bytes msg)))

    (: client-identity (-> (U String False) String))
    (define/public (client-identity identity)
      (cond
        [identity
          (set! current-client identity)
          identity]
        [else
          (set! current-client (send database create-client))
          current-client]))

    (: send-changes (-> Void))
    (define/public (send-changes)
      (define changes (reduce-changes (map row->change (get-changes synced-revision current-client))))
      (define rev (send database current-revision))
      (send-message
        `#hasheq((type . "changes")
                 (changes . ,changes)
                 (currentRevision . ,rev)
                 (partial . #f)))
      (set! synced-revision rev))

    (: subscribe (-> (U Integer False) Void))
    (define/public (subscribe revision)
      (set! synced-revision (if revision revision 0))
      (send-changes)
      (send database subscribe send-changes))

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
                    (database-change-data chg)
                    (database-change-client chg))]
                [(= (database-change-type chg) UPDATE)
                  (send database apply-partial-update
                    (database-change-table chg)
                    (database-change-key chg)
                    (database-change-data chg)
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
            (define srv-changes (reduce-changes (map row->change (send database fetch-changes base current-client))))
            (define resolved (resolve-conflicts changes srv-changes))
            (map (lambda ([chg : database-change])
              (cond
                [(= (database-change-type chg) CREATE)
                  (send database apply-create
                    (database-change-table chg)
                    (database-change-key chg)
                    (database-change-data chg)
                    (database-change-client chg))]
                [(= (database-change-type chg) UPDATE)
                  (send database apply-update
                    (database-change-table chg)
                    (database-change-key chg)
                    (database-change-data chg)
                    (database-change-client chg))]
                [(= (database-change-type chg) DELETE)
                  (send database apply-delete
                    (database-change-table chg)
                    (database-change-key chg)
                    (database-change-client chg))]))
              resolved)
            (void)]))))
  ))
