#lang typed/racket

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
  [subscribe (-> (U Integer False) Void)]
  [create-database-change (-> JSExpr database-change)]
  [sync-changes (-> (Listof database-change) Integer Boolean Void)]))

(: sync-server% : Sync-Server%)
(define sync-server%
  (class object%
    (init [connect : Websocket-Connection])

    (: conn Websocket-Connection)
    (define conn connect)

    (super-new)

    (: current-client (U False String))
    (define current-client #f)

    (: synced-revision Integer)
    (define synced-revision 0)

    (: database (Instance Sync-Database%))
    (define database (get-sync-database))

    (thread (thunk
      (sync (ws-connection-closed-evt conn))
      (logf 'debug "unsubscribing\n")
      (when current-client
        (send database unsubscribe (assert current-client)))))

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
          (assert current-client)]))

    (: get-send-changes (-> (-> Void)))
    (define/private (get-send-changes)
      (lambda ()
        ;; TODO confirm that the below is the correct way to do this...
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
        (set! synced-revision rev)))

    (: subscribe (-> (U Integer False) Void))
    (define/public (subscribe revision)
      (set! synced-revision (if revision revision 0))
      ((get-send-changes))
      (when current-client
        (send database subscribe (assert current-client) (get-send-changes))))

    ;; adds the current client to the given hash and converts to a database-change
    (: create-database-change (-> JSExpr database-change))
    (define/public (create-database-change chg)
      (define change (cast chg (HashTable Symbol JSExpr)))
      (define d-change (if (hash-has-key? change 'obj)
        (hash-set change 'data (jsexpr->string (hash-ref change 'obj)))
        change))
      (json->database-change (if (hash-has-key? d-change 'client)
        d-change
        (hash-set d-change 'client (assert current-client)))))

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
            (void)]))))
  ))
