(module format-trace racket
  (provide format-stack-trace)
  (define (format-stack-trace trace)
    (for/list ([item (in-list trace)])
      (format "~a at:\n  ~a\n"
              (if (car item)
                  (car item)
                  "<unknown procedure>")
              (if (cdr item)
                  (format "line ~a, column ~a, in file ~a"
                          (srcloc-line (cdr item))
                          (srcloc-column (cdr item))
                          (srcloc-source (cdr item)))
                  "<unknown location>")))))
