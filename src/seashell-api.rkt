(module seashell-api racket
  (require web-server/servlet
           web-server/servlet-env
           net/base64
           openssl/sha1
           racket/sandbox
           json
           "log.rkt"
           "common.rkt"
           "config.rkt"
           "database.rkt")
  (provide start-api)

  (define db (db-connect seashell-db-host seashell-db-port))

  (define (generate-session-key)
    (bytes->string/utf-8
     (base64-encode
      (list->bytes
       (map (lambda(u) (random 255))
            (build-list 32 identity)))
      #"")))
  
  (define (has-credentials? req-creds cred-list)
    (and
     (andmap
      (lambda(cred)
        (findf (lambda(x) (equal? x cred)) cred-list))
      req-creds)
     #t))
  
  ;; API call which authenticates a user.
  (define (do-api-authenticate args uid)
    (define (api-authenticate-user user pass)
      (let* ((users
              (db-get-every-keys db 'api_user '(id name passwd cred_list)))
             (user
              (findf (lambda(u) (equal? (hash-ref u 'name "noname") user)) users)))
        (if
         (and (hash? user)
              (equal?
               (with-input-from-string
                pass
                (thunk (sha1-bytes (current-input-port))))
               (hash-ref user 'passwd #"")))
         (values (hash-ref user 'id)
                 (hash-ref user 'cred_list empty))
         (values #f #f))))
    (match args
      [(hash-table ('key (? string? key))
                   (args `(,user ,pass)))
       (let*-values
           (((session-entry) (db-get db 'session key))
            ((session)
             (if (and (hash? session-entry)
                      (< (current-seconds)
                         (+ api-session-timeout
                            (hash-ref session-entry 'time_opened 0))))
                 session-entry #f))
            ((auth-uid auth-cred-list) (api-authenticate-user user pass)))
         (if (and session
                  auth-uid)
             (begin
               (db-set-keys db 'session key
                            `((time_opened . ,(current-seconds))
                              (cred_list . ,auth-cred-list)
                              (user . ,auth-uid)))
               (logf 'info "API user ~a (uid=~a) successfully authenticated." user auth-uid)
               `((status . #t)
                 (result . #t)))
             (begin
               (logf 'info "Invalid API credentials presented for user ~a." user)
               `((status . #t)
                 (result . #f)))))]
      [else `((status . #t) (result . #f))]))
  
  ;; Destroy a user session.
  (define (destroy-session args uid)
    (match args
      [(hash-table ('key (? string? key)) _ ...)
       (db-remove db 'session key)
       (clear-continuation-table!)
       `((status . #t)
         (result . #t))]
      [else `((status . #t)
              (result . #f))]))
  
  ;; Bind an API call to a continuation.
  (define (call/bind name proc required-creds direct embed)
    (define (convert req)
      (with-handlers
          ([exn:fail? (lambda(e) (make-hash))])
        (bytes->jsexpr (request-post-data/raw req))))
    (define (verify-auth conv-req)
      (match conv-req
        [(hash-table ('key (? string? key)) _ ...)
         (let*
             ((session-entry (db-get db 'session key))
              (session
               (if (and (hash? session-entry)
                        (< (current-seconds)
                           (+ api-session-timeout
                              (hash-ref session-entry 'time_opened 0))))
                   session-entry 'nosession)))
           (if (hash? session)
               (values
                (has-credentials? required-creds
                                  (hash-ref session 'cred_list empty))
                (hash-ref session 'user #f))
               (values session #f)))]
        [else
         (logf 'info "Could not authenticate: malformed request ~a" conv-req)
         (values #f #f)]))
    `(,name . ,(embed
                (lambda(req)
                  (let*-values
                      (((conv-req) (convert req))
                       ((auth-status auth-uid) (verify-auth conv-req)))
                    (if
                     (and (hash? conv-req)
                          (or (hash-has-key? conv-req 'args) direct)
                          (equal? auth-status #t))
                     ((if direct
                          response/json
                          (lambda(ret)
                            (response/json
                             `((status . #t)
                               (result . ,ret)))))
                      (proc
                       (if direct
                           conv-req
                           (hash-ref conv-req 'args))
                       auth-uid))
                     (begin
                       (logf 'info "API access denied for method ~a, request=~a."
                             name conv-req)
                       (response/json
                        (if
                         auth-status
                         `((status . #f))
                         `((status . #f) (access . #f)))))))))))
  
  ;; Return initial continuation table for API calls.
  (define (new-api-entry req)
    (let
        ((session-key (generate-session-key)))
      (db-set db 'session session-key
              `((time_opened . ,(current-seconds))
                (cred_list . ())))
      (send/suspend/dispatch
       (lambda(embed/url)
         (response/json
          `((key . ,session-key)
            (k . ,(make-hash (gen-continuation-table embed/url)))))))))
  
  (define (get-api-user-name args uid)
    (if uid
        (hash-ref (db-get-keys db 'api_user uid '(full_name)) 'full_name "")
        ""))
  
  ;; Hash table of log tailers. TODO collect these.
  (define session-log-table (make-hash))
  
  ;; Block on reading a log and return one entry.
  (define (tail-log log args uid)
    (match args
      [(hash-table ('key (? string? key)) _ ...)
       (let
           ((logger
             (if (hash-has-key? session-log-table
                                (cons key log))
                 (hash-ref session-log-table
                           (cons key log))
                 (let
                     ((chan (make-channel)))
                   (thread
                    (thunk
                     (let loop ((listener (make-log-reader log)))
                       (channel-put chan (listener))
                       (loop listener))))
                   (hash-set! session-log-table
                              (cons key log)
                              chan)
                   chan))))
         (with-handlers
             ([exn:fail:resource?
               (lambda(e) `((status . #t) (result . #f)))])
           (call-with-limits 120 #f
                             (thunk
                              `((status . #t)
                                (result . ,(channel-get logger)))))))]
      [else `((status . #t) (result . #f))]))
  
  ;; Generate the client's continuation table for anonymous calls.
  (define (gen-continuation-table embed/url)
    ;; List of available API calls and associated functions.
    `(,(call/bind 'isValidSession  (lambda(x u) #t)                     '()    #f embed/url)
      ,(call/bind 'destroySession  destroy-session                      '()    #t embed/url)
      ,(call/bind 'tailLogs        ((curry tail-log) "^.*$")            '(adm) #t embed/url)
      ,(call/bind 'authenticate    do-api-authenticate                  '()    #t embed/url)
      ,(call/bind 'getUserName     get-api-user-name                    '()    #f embed/url)))

  (define (start-api req)
    (match (request-path-string req)
      [(regexp #rx"^/api/init.*$")
       (new-api-entry req)])))
