(module generic-rpc racket
  ;; Generic FASL-passing RPC for Racket.
  ;; Provides two routine. When start-server is invoked,
  ;; the process will listen on hostname:port. For each
  ;; connection, a thread is created and proc is invoked on
  ;; fasl->s-exp applied to the received data. When proc
  ;; returns, s-exp->fasl is applied to the value, and
  ;; the result is sent back to the client. The thread then
  ;; closes all connections and terminates.
  ;; When remote-call is invoked on a port, hostname, and expression,
  ;; a connection is established with the remote host and the remote
  ;; procedure is invoked on the expression.
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
