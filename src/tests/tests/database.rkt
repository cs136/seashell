#lang racket

(require seashell/db/database
         seashell/db/seashell
         rackunit)

(define/provide-test-suite database-suite
  (test-suite "Database suite"
    (test-case "Basic sync database functions"
      (define foo (new sync-database% [path 'memory]))
      (send foo apply-create "test" "12345" (hash 'bar (hash)) #f)
      (send foo apply-update "test" "12345" (hash 'foo.baz 2) #f)
      (define res (send foo fetch "test" "12345"))
      (check-equal? res `#hasheq((bar . #hasheq())
                                 (foo . #hasheq((baz . 2)))))
      (send foo apply-partial-update "test" "12345" (hash 'foo.baz 10) #f)
      (send foo apply-partial-delete "test" "X")
      (define res2 (send foo fetch "test" "12345"))
      (check-equal? res2 `#hasheq((bar . #hasheq())
                                  (foo . #hasheq((baz . 2)))))
      (send foo apply-partials 3)
      (define res3 (send foo fetch "test" "12345"))
      (check-equal? res3 `#hasheq((bar . #hasheq())
                                  (foo . #hasheq((baz . 10))))))
    ))
