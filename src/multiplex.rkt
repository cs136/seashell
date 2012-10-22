(module multiplex racket
  ;; Message multiplexer. Communication channels are unlimited async channels.
  (require racket/async-channel)
  (provide make-multiplexer mt-subscribe mt-receive mt-send)
  ;; Clients are pairs of (thread . channel)
  (define (multiplex-loop clients)
    (match (thread-receive)
      [(list 'subscribe (? thread? thd) (? channel? resp-ch))
       (let ((new-chan (make-async-channel #f)))
         (channel-put resp-ch new-chan)
         (multiplex-loop
          (cons (cons thd new-chan) clients)))]
      [(var next-msg)
       (multiplex-loop
        (filter-map
         (lambda(client)
           (if (thread-dead? (car client))
               #f
               (begin
                 (async-channel-put (cdr client) next-msg)
                 client)))
         clients))]))
  (define (make-multiplexer)
    (thread (thunk (multiplex-loop '()))))
  (define (mt-subscribe thd)
    (let ((resp-ch (make-channel)))
      (thread-send thd `(subscribe ,(current-thread) ,resp-ch))
      (channel-get resp-ch)))
  (define mt-receive async-channel-get)
  (define mt-send thread-send))
