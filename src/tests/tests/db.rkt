#lang at-exp racket

(require rackunit
         seashell/seashell-config
         seashell/db/support
         seashell/db/changes
         seashell/db/updates
         seashell/db/database
         json)

(define/provide-test-suite database-suite
  (test-suite "Database (SQLite) Suite"
    (test-case "Test set-by-key-path"
      (check-equal? (set-by-key-path @~a{{}} "foo" @~a{"value"}) "{\"foo\":\"value\"}")
      (check-equal? (set-by-key-path @~a{{}} "foo.bar" @~a{"value"}) "{\"foo\":{\"bar\":\"value\"}}"))

    (test-case "Test apply-modifications"
      (check-equal? (string->jsexpr (apply-modifications @~a{{"foo": "oldValue"}} @~a{{"foo": "newValue", "bar":2}}))
                     '#hasheq((bar . 2) (foo . "newValue"))))

    (test-case "Test fold-create-and-update"
      (check-equal? (string->jsexpr (fold-create-and-update @~a{{"foo": "value"}} @~a{{"foo": "value2", "bar": "new Value"}}))
                     '#hasheq((bar . "new Value") (foo . "value2"))))

    (test-case "Test fold-update-and-update"
      (check-equal?
         (string->jsexpr (fold-update-and-update @~a{{"foo": "bar"}} @~a{{"bar": "baz"}}))
          '#hasheq((bar . "baz") (foo . "bar")))
      (check-equal?
         (string->jsexpr (fold-update-and-update @~a{{"foo": "bar"}} @~a{{"foo": "baz"}}))
          '#hasheq((foo . "baz")))
      (check-equal?
         (string->jsexpr (fold-update-and-update @~a{{"foo": {"bar": "baz", "baz": "bar"}}} @~a{{"foo.bar": "foobar"}}))
          '#hasheq((foo . #hasheq((baz . "bar") (bar . "foobar")))))
      (check-equal?
         (string->jsexpr (fold-update-and-update @~a{{"foo.bar": "foobar"}} @~a{{"foo": {"bar": "baz"}}}))
          '#hasheq((foo . #hasheq((bar . "baz"))))))

    (test-case "Test reduce-changes"
      (check-equal?
         (reduce-changes (list (database-change CREATE "_test" "foo" "1" "{}")
                                                      (database-change UPDATE "_test" "foo" "2" "{}")
                                                                             (database-change DELETE "_test" "bar" "1" "{}")))

          (hash
              '("foo" . "1")
                (database-change 1 "_test" "foo" "1" "{}")
                  '("foo" . "2")
                    (database-change 2 "_test" "foo" "2" "{}")
                      '("bar" . "1")
                        (database-change 3 "_test" "bar" "1" "{}"))
           )
      (check-equal?
         (reduce-changes (list (database-change CREATE "_test" "foo" "1" "{}")
                                                      (database-change CREATE "_test" "foo" "1" @~a{{"foo": "bar"}})))

          (hash '("foo" . "1") (database-change 1 "_test" "foo" "1" "{\"foo\": \"bar\"}"))
           )
      (check-equal?
         (reduce-changes (list (database-change CREATE "_test" "foo" "1" "{}")
                                                      (database-change DELETE "_test" "foo" "1" "false")))

          (hash '("foo" . "1") (database-change 3 "_test" "foo" "1" "false"))
           )
      (check-equal?
         (reduce-changes (list (database-change UPDATE "_test" "foo" "1" @~a{{"foo": "bar"}})
                                                      (database-change CREATE "_test" "foo" "1" @~a{{"foo": "bar baz"}})))
          (hash '("foo" . "1") (database-change 1 "_test" "foo" "1" "{\"foo\": \"bar baz\"}")))
      (check-equal?
         (reduce-changes (list (database-change UPDATE "_test" "foo" "1" "{}")
                                                      (database-change DELETE "_test" "foo" "1" "false")))
          (hash '("foo" . "1") (database-change 3 "_test" "foo" "1" "false"))
           )
      (check-equal?
         (reduce-changes (list (database-change UPDATE "_test" "foo" "1" @~a{{"foo": "baz"}})
                                                      (database-change UPDATE "_test" "foo" "1" @~a{{"title": "bar"}})))
          (hash
              '("foo" . "1")
                (database-change 2 "__SERVER_MERGED_CHANGE" "foo" "1" "{\"foo\":\"baz\",\"title\":\"bar\"}")))
      (check-equal?
         (reduce-changes (list (database-change DELETE "_test" "foo" "1" "false")
                                                      (database-change CREATE "_test" "foo" "1" @~a{{"foo": "bar"}})))
          (hash '("foo" . "1") (database-change 1 "_test" "foo" "1" "{\"foo\": \"bar\"}")))
      (check-equal?
         (reduce-changes (list (database-change DELETE "_test" "foo" "1" "false")
                                                      (database-change DELETE "_test" "foo" "1" "false")))
          (hash '("foo" . "1") (database-change 3 "_test" "foo" "1" "false")))
      (check-equal?
         (reduce-changes (list (database-change DELETE "_test" "foo" "1" "false")
                                                      (database-change UPDATE "_test" "foo" "1" @~a{{"foo": "bar"}})))
          (hash '("foo" . "1") (database-change 3 "_test" "foo" "1" "false"))))

    (test-case "Test resolve-conflicts"
      (check-equal?
         (resolve-conflicts (list (database-change CREATE "_test" "foo" "1" "{}")) '())
          (list (database-change 1 "_test" "foo" "1" "{}")))
      (check-equal?
         (resolve-conflicts '() (list (database-change 1 "_test" "foo" "1" "{}")))
          '())
      (check-equal?
         (resolve-conflicts (list (database-change UPDATE "_test" "foo" "1" "{}"))
                                                (list (database-change DELETE "_test" "foo" "1" "false")))
          '())
      (check-equal?
         (resolve-conflicts (list (database-change CREATE "_test" "foo" "1" @~a{{"foo": "bar", "foobar": "foobar"}}))
                                                (list (database-change UPDATE "_test" "foo" "1" @~a{{"foo": "baz", "bar": "bar"}})))
          (list
              (database-change 1 "_test" "foo" "1" "{\"foo\":\"baz\",\"foobar\":\"foobar\",\"bar\":\"bar\"}")))
      (check-equal?
         (resolve-conflicts (list (database-change DELETE "_test" "foo" "1" "false"))
                                                (list (database-change UPDATE "_test" "foo" "1" @~a{{"foo": "baz", "bar": "bar"}})))
          (list (database-change 3 "_test" "foo" "1" "false")))
      (check-equal?
         (resolve-conflicts (list (database-change UPDATE "_test" "foo" "1" @~a{{"foo" : "bar"}}))
                                                (list (database-change UPDATE "_test" "foo" "1" @~a{{"foo" : "baz"}})))
          '())
      (check-equal?
         (resolve-conflicts (list (database-change UPDATE "_test" "foo" "1" @~a{{"foo.bar" : "bar"}}))
                                                (list (database-change UPDATE "_test" "foo" "1" @~a{{"foo" : "baz"}})))
          '())
      (check-equal?
         (resolve-conflicts (list (database-change UPDATE "_test" "foo" "1" @~a{{"foo" : "bar", "foobar": "foobar"}}))
                                                (list (database-change UPDATE "_test" "foo" "1" @~a{{"foo" : "baz", "bar": "bar"}})))
          (list (database-change 2 "_test" "foo" "1" "{\"foobar\":\"foobar\"}")))
      (check-equal?
         (resolve-conflicts (list (database-change UPDATE "_test" "foo" "1" @~a{{"foo" : "bar"}}))
                                                (list (database-change UPDATE "_test" "foo" "1" @~a{{"foo.bar" : "bar"}})))
          (list (database-change 2 "_test" "foo" "1" "{\"foo\":\"bar\"}"))))

    (test-case "Basic sync-database functions"
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
