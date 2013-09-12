#lang racket

(require seashell/websocket
         racket/async-channel)

(define (conn-dispatch wsc header-resp)
  (ws-send wsc #"Hello!")
  (let loop ()
    (match (sync wsc))
      [(? seashell-websocket-connection?)
       (define data (ws-recv wsc))
       (printf "Received message '~a'~n" data)
       (ws-send wsc (bytes-append #"You said: " data))
       (loop)]
      [(var x)
       (printf "Received something strange: ~a~n" x)
       (ws-close! wsc)
       (ws-destroy! wsc)])))

(define conf-chan (make-async-channel))

(define shutdown-server
  (seashell-websocket-serve
    conn-dispatch
    #:port 0
    #:listen-ip "0.0.0.0"
    #:max-waiting 4
    #:timeout (* 60 60)
    #:confirmation-channel conf-chan))

(printf "Listening on port ~a~n" (async-channel-get conf-chan))

(with-handlers ([exn:break? (lambda(e) (printf "Break~n"))])
               (sync/enable-break never-evt))

(printf "Quitting.~n")
