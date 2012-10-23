(module seashell-api racket
  (require web-server/servlet
           web-server/servlet-env
           net/base64
           "common.rkt"
           "config.rkt"
           "database.rkt")
  (provide start-api)

  (define db (db-connect seashell-db-host seashell-db-port))

  (define (generate-session-key)
    (base64-encode
      (list->bytes
        (map (lambda(u) (random 255))
          (build-list 32 identity)))
      #""))

  (define (handle-login req)
    ;; More login logic goes here.
    ;; XXX TODO security considerations
    ;; If this point is reached, the user is considered to be authenticated
    ;;  and authorized to use the program. So:
    ;;    (1) Generate a session key.
    ;;    (2) Store the session key with the Quest user ID in the database.
    ;;      TODO Storage backing the db must be secure.
    ;;      TODO Don't leak function arguments in stack traces.
    ;;    (3) Respond with the session key. The client is responsible for
    ;;        storing the session key.

    ;; Once authenticated, the client begins a set of interactions with the server
    ;; by exchanging the session key for a continuation table which defines
    ;; the API provided by the server. This process is agnostic of client.

    ;; The server may respond to an API call with an 'unknown continuation' message.
    ;; In this case, the client must present the session key again in hopes of receiving
    ;; a new continuation table. If no table is provided, then the session is considered
    ;; terminated by the server.
    (let
       ((key (bytes->string/utf-8 (generate-session-key)))
        (user "ctdalek"))
      (db-set db 'session key `((user . ,user) (login_time . ,(current-seconds))))
      (response/json
        `((session-key . ,key)))))

  ;; Generate the client's continuation table.
  (define (gen-continuation-table embed/url)
    `((save . ,(embed/url do-save))
      (open . ,(embed/url do-open))
      (run  . ,(embed/url do-run))))

  ;; Exchange a valid session key for a continuation table.
  (define (handle-exchange req)
    ;; If the session key exists in the database, provide the continuation table
    ;; with no further checks.
    (let
       ((bdg-skey (bindings-assq #"key"
                   (force (request-bindings/raw-promise req)))))
      (if (and bdg-skey
               (hash? (db-get db 'session (bytes->string/utf-8
                                           (binding:form-value bdg-skey)))))
          ;; Valid session.
          (send/suspend/dispatch
            (lambda(embed/url)
              (response/json (gen-continuation-table embed/url))))
          ;; No valid session.
          (send/back
            ;; TODO standardize error messages?
            (response/json '((status . #f)))))))

  ;; Some examples.
  ;;;;;;;;;;;;;;;;;
  (define (do-save req)
    ;; save the document here.
    (printf "do-save~n")
    (do-save
     (send/suspend
      (lambda(url)
        (response/json
          `((k . ,(make-hash `((save . ,url))))))))))

  (define (do-open req)
    (define (do-open-r req n)
      ;; open the document here.
      (printf "do-open ~a~n" n)
      (do-open-r
       (send/suspend
         (lambda(url)
           (response/json
             `((k . ,(make-hash `((open . ,url))))))))
       (+ n 1)))
    (do-open-r req 0))

  (define (do-run req)
    (printf "do-run~n")
    (send/suspend
      (lambda(url)
        (response/json
          `((run . ,url))))))
  ;;;;;;;;;;;;;;;

  (define (start-api req)
    (match (request-path-string req)
      [(regexp #rx"^/api/login.*$")
       (handle-login req)]
      [(regexp #rx"^/api/k.*$")
       (handle-exchange req)])))
