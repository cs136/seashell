(module seashell-log racket
  (require "multiplex.rkt"
           racket/date)
  (provide logf make-log-reader make-fs-logger)
  (define log-mtx (begin0 (make-multiplexer)
                          (printf "Initialized log multiplexer.~n")))
  (define log-ts-str "[~a-~a-~a ~a:~a:~a ~a]")
  (define (log-ts-args)
    (define (pad-left z i)
       (string-append (make-string (- z (string-length (number->string i))) #\0)
                      (number->string i)))
    (let ((dt (seconds->date (current-seconds))))
      (list
       (pad-left 4 (date-year dt))
       (pad-left 2 (date-month dt))
       (pad-left 2 (date-day dt))
       (pad-left 2 (date-hour dt))
       (pad-left 2 (date-minute dt))
       (pad-left 2 (date-second dt))
       (string-append (number->string (quotient (date-time-zone-offset dt) 3600))
                      (pad-left 2 (remainder (date-time-zone-offset dt) 3600))))))
  
  ;; logf: category format args... -> void
  (define logf
    (lambda(cat fmt . args)
      (mt-send
       log-mtx
       (cons cat
             (with-output-to-string
              (thunk
               (apply printf `(,(string-append log-ts-str " ~a: " fmt)
                               ,@(log-ts-args)
                               ,cat
                               ,@args))))))))
  
  ;; make-log-reader: type-regexp -> (func: -> message)
  (define (make-log-reader type-regexp)
    (define chan (mt-subscribe log-mtx))
    (define (next-message)
      (match
          (let
              ((msg (mt-receive chan)))
            (cons (symbol->string (car msg)) (cdr msg)))
        [(cons (regexp type-regexp) (var msg)) msg]
        [else (next-message)]))
    next-message)
  
  ;; make-fs-logger: type-regexp file -> thread
  (define (make-fs-logger type-regexp file)
    (define reader (make-log-reader type-regexp))
    (thread
     (thunk
      (call-with-output-file file
        (lambda(port)
         (let loop ()
           (fprintf port "~a~n" (reader))
           (flush-output port)
           (loop)))
        #:exists 'append)))))
