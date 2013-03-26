;; Generic RPC library for Racket
;; Copyright (C) 2012-2013 Marc Burns
(module generic-rpc racket
  (require racket/tcp racket/serialize)
  (provide start-server remote-call)

  (define
    (start-server hostname port proc)
    (let
        ((listener
          (tcp-listen port 10 #t hostname)))
      (with-handlers
          ([exn:fail:network? (lambda(e) (void))])
        (do () (#f)
          (let-values (((client-in client-out)
                        (tcp-accept listener)))
            (thread
             (thunk
              (handle-client client-in client-out proc))))))))

  (define
    (handle-client client-in client-out proc)
    (with-handlers
        ([exn? (lambda(e)
                 (close-input-port client-in)
                 (close-output-port client-out)
                 (raise e))])
      (write
       (serialize
        (proc
         (begin0
           (deserialize (read client-in))
           (close-input-port client-in))))
       client-out)
      (close-output-port client-out)))

  (define
    (remote-call hostname port expr)
    (let-values
        (((server-in server-out)
          (tcp-connect hostname port)))
      (write (serialize expr)
             server-out)
      (tcp-abandon-port server-out)
      (begin0
        (deserialize (read server-in))
        (close-input-port server-in)))))
