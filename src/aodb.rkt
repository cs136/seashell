(module aodb racket
  ;; FFI module for aodb.c
  (require ffi/unsafe
           ffi/unsafe/define
           ffi/unsafe/alloc)
  (provide db_init
           db_read
           db_release
           db_write
           db_close
           record-data
           record-timestamp)
  
  (define-ffi-definer define-aodb (ffi-lib "./aodb.so"))
  
  ;; Wrap structures declared in aodb.c
  (define-cstruct _db_context
    ([fd_read _int]
     [fd_append _int]))
  (define-cstruct _db_record
    ([magic _int]
     [tv_sec _int]
     [tv_nsec _int]
     [length _int]
     [data_ptr _pointer])
    #:alignment 1)
  
  (define-struct record (timestamp data ptr))
  (define-aodb db_init (_fun (o : (_ptr o _db_context))
                             _string/utf-8
                             -> (r : _int)
                             -> (values o r))
    #:wrap (lambda(proc)
             (lambda(path)
               (let-values (((handle res) (proc path)))
                 (if (zero? res) handle #f)))))
  (define-aodb db_close (_fun _db_context-pointer -> _void))
  (define-aodb db_read (_fun _db_context-pointer
                             (o : (_ptr o _db_record-pointer/null))
                             -> (r : _int)
                             -> (values o r))
    #:wrap (lambda(proc)
             (lambda(ctx)
               (let-values (((rec res) (proc ctx)))
                 (match res
                   [0 (make-record
                       (+ (* 1000000000 (db_record-tv_sec rec)) (db_record-tv_nsec rec))
                       (make-sized-byte-string (db_record-data_ptr rec)
                                               (db_record-length rec))
                       rec)]
                   [-1 #f]
                   [-2 (error 'database "Database exception on read.")]
                   [else (error 'database "Bad value returned from aodb: ~a" res)])))))
  (define (db_release rec) (free (record-ptr rec)))
  (define-aodb db_write (_fun _db_context-pointer _bytes _int -> _int)
    #:wrap (lambda(proc)
             (lambda(ctx bytes)
               (if (zero? (proc ctx bytes (bytes-length bytes)))
                   (void)
                   (error 'database "Database exception on write."))))))
