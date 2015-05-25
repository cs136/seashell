#lang racket
(provide (all-defined-out))
(require scribble/manual
         scribble/struct
         scribble/decode
         scribble/scheme)

(define (seashell-title #:tag [tag #f] #:tag-prefix [prefix #f] #:style [style #f]
                        #:date [date #f]
               . str)
  (apply title str #:tag tag #:tag-prefix prefix #:style style #:date date #:version (vector-ref (current-command-line-arguments) 0)))

;; Source-URL handling functions
(define (source-url path)
  (string-append "https://github.com/cs136/seashell/blob/master/" path))
(define (source-url-link path)
  (link (source-url path) path))
(define (pr n)
  (link (format "https://github.com/cs136/seashell/pull/~a" n) (format "#~a" n)))
(define (issue n)
  (link (format "https://github.com/cs136/seashell/issues/~a" n) (format "#~a" n)))

;; C/C++ FFI documentation
(define (as-cpp-defn name s)
  (make-target-element #f
                       (list (as-index s))
                       `(cpp ,(format "~a" name))))

(define-syntax (function stx)
  (syntax-case stx ()
    [(_ (ret name [type arg] ...) . body)
     #'(*function (cpp/sym 'ret)
                  (as-cpp-defn 'name (cpp/sym 'name))
                  (list (type/sym 'type) ...)
                  (list (var/sym 'arg) ...)
                  (lambda ()
                    (list . body)))]))

(define-syntax (subfunction stx)
  (syntax-case stx ()
    [(_ (ret name [type arg] ...) . body)
     #'(make-blockquote
        "leftindent"
        (flow-paragraphs
         (decode-flow
          (list
           (*function (cpp/sym 'ret)
                      (var/sym 'name)
                      (list (type/sym 'type) ...)
                      (list (var/sym 'arg) ...)
                      (lambda ()
                        (list . body)))))))]))

(define (to-flow elem)
  (make-flow (list (make-paragraph (list elem)))))

(define (*function ret name types args rest-thunk)
  (let ([spacer (hspace 1)]
        [pair-type (lambda (t v)
                     (if (equal? "..." (element->string t))
                         t
                         (make-element #f
                                       (list
                                        t
                                        (hspace 1)
                                        v))))]
        [super-long? ((+ (element-width ret)
                         1
                         (element-width name)
                         1
                         (apply max 0 (map (lambda (t v)
                                             (+ (element-width t)
                                                1
                                                (element-width v)))
                                           types
                                           args))
                         1)
                      . > .
                      65)])
    (make-splice
     (cons
      (boxed
       (make-table
        #f
        (append
         (if super-long?
             (list (list (to-flow ret) 'cont 'cont))
             null)
         (list
          (append
           (if super-long?
               null
               (list (to-flow ret)
                     (to-flow spacer)))
           (list (to-flow name)
                 (to-flow (tt "("))
                 (if (null? types)
                     (to-flow (tt ")"))
                     (to-flow (make-element
                               #f
                               (cons (pair-type (car types) (car args))
                                     (if (null? (cdr types))
                                         (list (tt ")"))
                                         (list (tt ","))))))))))
         (if (null? types)
             null
             (let loop ([types (cdr types)]
                        [args (cdr args)])
               (if (null? types)
                   null
                   (cons 
                    (append
                     (if super-long?
                         null
                         (list (to-flow spacer)
                               (to-flow spacer)))
                     (list (to-flow spacer)
                           (to-flow spacer)
                           (to-flow (make-element
                                     #f
                                     (cons
                                      (pair-type (car types) (car args))
                                      (if (null? (cdr types))
                                          (list (tt ")"))
                                          (list (tt ","))))))))
                    (loop (cdr types) (cdr args)))))))))
      (rest-thunk)))))

(define (boxed t)
  (make-table
   'boxed
   (list (list (make-flow (list t))))))

(define (cpp/sym s)
  (cpp (symbol->string s)))

(define (type/sym s)
  (cpp (regexp-replace* #rx"-" (symbol->string s) " ")))

(define (var/sym s)
  (*var (symbol->string s)))

(define cpp
  (case-lambda
   [(x)
    (if (string? x)
        (let ([e (tt x)])
          (make-delayed-element
           (lambda (r part ri)
             (let ([d (resolve-get/tentative part ri `(cpp ,x))])
               (list
                (if d
                    (make-link-element syntax-link-color (list e) `(cpp ,x))
                    e))))
           (lambda () e)
           (lambda () e)))
        (tt x))]
   [more (apply tt more)]))
   
(define cppi cpp)
(define cppdef (lambda (x) (as-cpp-defn x (cpp x))))
(define *var italic)
