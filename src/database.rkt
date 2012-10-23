(module database racket
  (require "aodb.rkt"
           "log.rkt")
  (provide start-database-server
           db-connect
           db-get
           db-get-keys
           db-get-every
           db-get-every-keys
           db-set
           db-set-keys
           db-remove
           db-insert
           db-show-types
           db-collect-garbage)
  ;; Log-based database. For extensibility, and because I don't
  ;; trust Racket not to destroy an output file, the reader and
  ;; writer are implemented in a separate C module.
  
  ;; The database itself associates symbols (types) to maps
  ;; between primary keys (arbitrary type) and values (hash tables).
  ;; Each table is a map between keys and values of arbitrary type.
  ;; Thus, each type of object stored in the database will have one
  ;; primary key, and each instance of any object is really an
  ;; arbitrary dictionary. This format should be flexible enough
  ;; for what Wildcard needs to implement, yet restrictive enough
  ;; to prevent a messy database design. The use of an arbitrary
  ;; dictionary as storage allows for data to be extended trivially.
  
  ;; Type metadata.
  (define default-meta-data (make-immutable-hash '((counter . 1))))
  
  ;; Database instance.
  (require racket/serialize)
  (define (db-instance file)
    (define ctx (db_init file))
    (when (false? ctx)
      (error 'database "Could not start database instance. See log."))
    (define-struct d-hash-rep-oa ())
    (define-struct db-offline-tag (fn))
    (define-struct db-cache (inner-k table) #:transparent)
    (define (d-hash-set ht k v)
      ;; stub.
      (hash-set ht k v))
    (define (d-hash-ref ht k [dv (make-d-hash-rep-oa)])
      ;; stub.
      (if (d-hash-rep-oa? dv)
          (hash-ref ht k)
          (hash-ref ht k dv)))
    (define (d-hash-remove ht k)
      ;; stub.
      (hash-remove ht k))
    (define (unique-key-for hash)
      ;; Generate a unique numeric key for hash.
      ;; Each object hash has a counter key.
      ;; Increment the value of the counter key and return
      ;; the previous value, or 1 if there was none.
      (let*
          ((meta (d-hash-ref hash 'type-meta-data default-meta-data))
           (new-count (add1 (d-hash-ref meta 'counter)))
           (new-meta (d-hash-set meta 'counter new-count)))
        (values new-meta new-count)))
    ;; Functionally modify the in-memory model of the database.
    ;; Evaluates to the modified database.
    ;; A query which modifies the database is either 'replace,
    ;; 'remove, 'set, or 'meta.
    ;;  'replace - Replace an object by key with a new set of keys and values.
    ;;  'remove - Remove an object by key from the database.
    ;;  'set - Set keys to values in an existing object by key, or create the
    ;;    object if no such key exists in the type.
    ;;  'meta - Set metadata about a type (e.g. unique key counter, lookup caches).
    (define (dispatch db exp)
      (match exp
        [`(replace ,type ,key ,data)
         (cache-set-assocs
          (d-hash-set db type
                      (d-hash-set
                       (d-hash-ref db type (make-immutable-hash))
                       key
                       (hash-remove
                        (make-immutable-hash data)
                        'id)))
          type key)]
        [`(remove ,type ,key)
         (if (hash-has-key?
              (d-hash-ref db type (make-immutable-hash))
              key)
             (d-hash-set db type
                         (d-hash-remove
                           (d-hash-ref db type (make-immutable-hash))
                           key))
             db)]
        [`(set ,type ,key ,data)
         (cache-set-assocs
          (d-hash-set db type
                      (d-hash-set
                       (d-hash-ref db type (make-immutable-hash))
                       key
                       (foldl
                        (lambda(assoc hash)
                          (if (equal? (car assoc) 'id)
                              hash
                              (d-hash-set hash (car assoc) (cdr assoc))))
                        (d-hash-ref
                         (d-hash-ref db type (make-immutable-hash))
                         key
                         (make-immutable-hash))
                        data)))
          type key)]
        [`(meta ,type ,meta-data)
         (d-hash-set db type
                     (d-hash-set
                      (d-hash-ref db type (make-immutable-hash))
                      'type-meta-data
                      meta-data))]))
    ;; Functionally query the database. Evaluates to the query
    ;; result. Queries are:
    ;;  'get-every - Return a list of every object in a given type.
    ;;  'get-every-keys - Return a list of every object in a certain type,
    ;;    where each object has been filtered to contain only the given keys.
    ;;  'get - Return a single object with given key in a given type.
    ;;  'get-keys - Return a single object with given key in a given type,
    ;;    where the object has been filtered to contain only the given keys.
    (define (query db exp)
      (match exp
        [`(get-every ,type)
         (filter-map
          (lambda(cell)
            (if (equal? (car cell) 'type-meta-data) #f
                (d-hash-set (cdr cell) 'id (car cell))))
          (hash->list (d-hash-ref db type (make-immutable-hash))))]
        [`(get-every-keys ,type ,keys)
         (map
          (lambda(obj)
            (make-immutable-hash
             (foldl
              (lambda(key assocs)
                (if (hash-has-key? obj key)
                    (cons (cons key
                                (d-hash-ref obj key))
                          assocs)
                    assocs))
              empty
              keys)))
          (filter-map
           (lambda(cell)
             (if (equal? (car cell) 'type-meta-data) #f
                 (d-hash-set (cdr cell) 'id (car cell))))
           (hash->list (d-hash-ref db type (make-immutable-hash)))))]
        [`(get ,type ,id)
         (let ((obj (d-hash-ref
                     (d-hash-ref db type
                                 (make-immutable-hash)) id #f)))
           (if obj
               (d-hash-set obj 'id id)
               #f))]
        [`(get-keys ,type ,id ,keys)
         (let ((obj (d-hash-ref
                     (d-hash-ref db type
                                 (make-immutable-hash)) id #f)))
           (if obj
               (d-hash-set
                (make-immutable-hash
                 (foldl
                  (lambda(key assocs)
                    (if (hash-has-key? obj key)
                        (cons (cons key
                                    (d-hash-ref obj key))
                              assocs)
                        assocs))
                  empty
                  keys))
                'id id)
               #f))]
        [`(show-types)
         (filter identity
                 (hash-map db
                           (lambda(k v) (if (and (hash? v) (< 0 (hash-count v)))
                                            k
                                            #f))))]))
    (define (cache-remove-assocs db type id)
      (let* ((typehash (d-hash-ref db type (make-immutable-hash)))
             (meta (d-hash-ref typehash 'type-meta-data default-meta-data))
             (caches (d-hash-ref meta 'caches '()))
             (object (d-hash-ref typehash id)))
        (hash-set
         db
         type
         (hash-set
          typehash
          'type-meta-data
          (hash-set
           meta
           'caches
           (map
            (lambda(cache)
              (if (hash-has-key? object (db-cache-inner-k cache))
                  (make-db-cache
                   (db-cache-inner-k cache)
                   (d-hash-remove cache (d-hash-ref object (db-cache-inner-k cache))))
                  cache))
            caches))))))
    (define (cache-set-assocs db type id)
      (let* ((typehash (d-hash-ref db type (make-immutable-hash)))
             (meta (d-hash-ref typehash 'type-meta-data default-meta-data))
             (caches (d-hash-ref meta 'caches '()))
             (object (d-hash-ref typehash id)))
        (hash-set
         db
         type
         (hash-set
          typehash
          'type-meta-data
          (hash-set
           meta
           'caches
           (map
            (lambda(cache)
              (if (hash-has-key? object (db-cache-inner-k cache))
                  (make-db-cache
                   (db-cache-inner-k cache)
                   (d-hash-set (db-cache-table cache) (d-hash-ref object (db-cache-inner-k cache)) id))
                  cache))
            caches))))))
    (define (generate-cache db type inner-key)
      (let* ((typehash (d-hash-ref db type (make-immutable-hash)))
             (meta (d-hash-ref typehash 'type-meta-data default-meta-data)))
        (d-hash-set
         db
         type
         (d-hash-set
          typehash
          'type-meta-data
          (d-hash-set
           meta
           'caches
           (cons
            (make-db-cache inner-key
                           (make-immutable-hash
                            (foldl
                             (lambda(c b)
                               (if (hash-has-key? (cdr c) inner-key)
                                   (cons
                                    (cons (d-hash-ref (cdr c) inner-key)
                                          (car c))
                                    b)
                                   b))
                             '()
                             (hash->list typehash))))
            (d-hash-ref meta 'caches '())))))))
    ;(d-hash-ref hash 'type-meta-data default-meta-data))
    ;; Load the database from the append-only log.
    (define (db-loader ht ctx)
      (let ((next-record (db_read ctx)))
        (if next-record
            (db-loader
             (dispatch ht
                       (begin0
                         (with-input-from-bytes
                          (record-data next-record)
                          (compose deserialize read))
                         (db_release next-record))) ctx)
            ht)))
    ;; Receive and process database queries and updates.
    ;; Tail-recursive, executed in a separate thread.
    ;; All requests include a channel on which a response must be sent,
    ;; even in case of error. This ensures that the RPC thread will
    ;; never deadlock.
    (define (db-loop db ctx)
      (match (thread-receive)
        [`(get-every ,type ,resp-ch)
         (channel-put resp-ch
                      (query db `(get-every ,type)))
         (db-loop db ctx)]
        [`(get-every-keys ,type ,keys ,resp-ch)
         (channel-put resp-ch
                      (query db `(get-every-keys ,type ,keys)))
         (db-loop db ctx)]
        [`(get ,type ,id ,resp-ch)
         (channel-put resp-ch
                      (query db `(get ,type ,id)))
         (db-loop db ctx)]
        [`(get-keys ,type ,id ,keys ,resp-ch)
         (channel-put resp-ch
                      (query db `(get-keys ,type ,id ,keys)))
         (db-loop db ctx)]
        [`(show-types ,resp-ch)
         (channel-put resp-ch
                      (query db `(show-types)))
         (db-loop db ctx)]
        [`(collect-garbage ,resp-ch)
         (collect-garbage)
         (channel-put resp-ch (void))
         (db-loop db ctx)]
        ;; As a side-effect, this function appends changes to the log file
        ;; representing the database. If an exception is encountered during
        ;; the request, the request is cleanly aborted with no change to the
        ;; database or log.
        [`(set ,type ,id ,assocs ,resp-ch)
         (db-loop
          (with-handlers
              ([exn? (lambda(e) (logf 'exception "Request generated database exception: ~a ~a"
                                        `(set ,type ,id ,assocs ,resp-ch) e)
                       (channel-put resp-ch (void))
                       db)])
            (let ((new-db
                   (dispatch db `(replace ,type ,id ,assocs))))
              (db_write ctx (with-output-to-bytes (lambda()(write (serialize `(replace ,type ,id ,assocs))))))
              (channel-put resp-ch (void))
              new-db))
          ctx)]
        [`(insert ,type ,assocs ,resp-ch)
         (db-loop
          (with-handlers
              ([exn? (lambda(e) (logf 'exception "Request generated database exception: ~a ~a"
                                        `(insert ,type ,assocs ,resp-ch) e)
                       (channel-put resp-ch #f)
                       db)])
            (let*-values
                (((new-meta new-id)
                  (unique-key-for (d-hash-ref db type (make-immutable-hash))))
                 ((new-db)
                  (dispatch
                   (dispatch
                    db
                    `(meta ,type ,new-meta))
                   `(replace ,type ,new-id ,assocs))))
              (db_write ctx (with-output-to-bytes (lambda()(write (serialize `(meta ,type ,new-meta))))))
              (db_write ctx (with-output-to-bytes (lambda()(write (serialize `(replace ,type ,new-id ,assocs))))))
              (channel-put resp-ch new-id)
              new-db))
          ctx)]
        [`(set-keys ,type ,id ,assocs ,resp-ch)
         (db-loop
          (with-handlers
              ([exn? (lambda(e) (logf 'exception "Request generated database exception: ~a ~a"
                                        `(set-keys ,type ,id ,assocs) e)
                       (channel-put resp-ch (void))
                       db)])
            (let ((new-db
                   (dispatch db `(set ,type ,id ,assocs))))
              (db_write ctx (with-output-to-bytes (lambda()(write (serialize `(set ,type ,id ,assocs))))))
              (channel-put resp-ch (void))
              new-db))
          ctx)]
        [`(remove ,type ,id ,resp-ch)
         (db-loop
          (with-handlers
              ([exn? (lambda(e) (logf 'exception "Request generated database exception: ~a ~a"
                                        `(remove ,type ,id) e)
                       (channel-put resp-ch (void))
                       db)])
            (let ((new-db
                   (dispatch db `(remove ,type ,id))))
              (db_write ctx (with-output-to-bytes (lambda()(write (serialize `(remove ,type ,id))))))
              (channel-put resp-ch (void))
              new-db))
          ctx)]
        [`(cached-lookup ,type ,cell ,resp-ch)
         (db-loop
          (let ((new-db (generate-cache db type (car cell))))
            (pretty-print new-db)
            (channel-put resp-ch (void))
            new-db)
          ctx)]
        [else
         (error 'database "db-loop: Unexpected message.")]))
    (logf 'info "Loading database...")
    (define db (db-loader (make-immutable-hash) ctx))
    (logf 'info "Database loaded.")
    (db-loop db ctx))
  
  ;; Database server.
  ;; This thread becomes an RPC server. Requests received here
  ;; are forwarded to the db-loop routine thru a thread mailbox.
  (require "generic-rpc.rkt")
  (define (start-database-server file hostname port)
    (define (database-serve-request req db-thread)
      (let ((response-ch (make-channel)))
        (thread-send db-thread `(,@req ,response-ch))
        (channel-get response-ch)))
    (define db-thread
      (thread (thunk (db-instance file))))
    (start-server hostname port
                  (lambda(req)
                    (database-serve-request req db-thread))))
  
  ;; Client functions.
  (define-struct db-info (host port))
  (define (db-connect host port)
    (make-db-info host port)) ;; This could keep the connection open.
  
  ;; db-get: db-info type id -> hash-map or #f
  ;; Returns object of key id and type type, or #f if
  ;; no such object exists.
  (define (db-get db type id)
    (remote-call (db-info-host db) (db-info-port db)
                 `(get ,type ,id)))
  
  ;; db-get-keys: db-info type id assocs -> hash-map or #f
  ;; Returns object of key id and type type filtered to include
  ;; only keys keys, or #f if no such object exists.
  (define (db-get-keys db type id keys)
    (remote-call (db-info-host db) (db-info-port db)
                 `(get-keys ,type ,id ,keys)))
  
  ;; db-get-every: db-info type -> list of hash-map
  ;; Returns all objects of type type.
  (define (db-get-every db type)
    (remote-call (db-info-host db) (db-info-port db)
                 `(get-every ,type)))
  
  ;; db-get-every-keys: db-info type assocs -> list of hash-map
  ;; Returns all objects of type type, each object filtered to
  ;; include only keys keys.
  (define (db-get-every-keys db type keys)
    (remote-call (db-info-host db) (db-info-port db)
                 `(get-every-keys ,type ,keys)))
  
  ;; db-set: db-info type id assocs -> void
  ;; Create or replace the object of type type and id id with
  ;; one containing exactly data.
  (define (db-set db type id data)
    (remote-call (db-info-host db) (db-info-port db)
                 `(set ,type ,id ,data)))
  
  ;; db-set-keys: db type id assocs -> void
  ;; Create or update the object of type type and id id s.t.
  ;; all keys, values in data are associated in the object.
  (define (db-set-keys db type id data)
    (remote-call (db-info-host db) (db-info-port db)
                 `(set-keys ,type ,id ,data)))
  
  
  ;; db-remove: db type id -> void
  ;; If an object with type type and id id exists, remove it.
  (define (db-remove db type id)
    (remote-call (db-info-host db) (db-info-port db)
                 `(remove ,type ,id)))
  
  ;; db-insert: db type assocs -> void
  ;; Generate an arbitrary unique numeric key for type type, and
  ;; create an object with this key associating the keys -> values
  ;; present in data.  The generated key will be unique if there are
  ;; no integer-valued keys in the store not resulting from calls
  ;; to db-insert.
  (define (db-insert db type data)
    (remote-call (db-info-host db) (db-info-port db)
                 `(insert ,type ,data)))
  
  ;; db-show-types: db -> list of symbols
  ;; Return a list of all non-empty symbols in database.
  (define (db-show-types db)
    (remote-call (db-info-host db) (db-info-port db)
                 `(show-types)))
  
  ;; db-cached-lookup: db type (cons k v) -> id
  ;; Finds object of given type containing association (k . v).
  ;; Returns the object key. If more than one such object exists,
  ;; one is selected pseudorandomly.
  (define (db-cached-lookup db type cell)
    (remote-call (db-info-host db) (db-info-port db)
                 `(cached-lookup ,type ,cell)))

  ;; db-collect-garbage: db -> void
  ;; Return a list of all non-empty symbols in database.
  (define (db-collect-garbage db)
    (remote-call (db-info-host db) (db-info-port db)
                 `(collect-garbage))))
