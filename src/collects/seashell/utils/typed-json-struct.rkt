#lang typed/racket
;; Seashell - a C Development Environment.
;; Copyright (C) 2016 The Seashell Maintainers.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; See also 'ADDITIONAL TERMS' at the end of the included LICENSE file.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This library provides helper macros for dealing with JSON data types.
(require typed/json
         (for-syntax racket/syntax))

(provide (struct-out exn:fail:json-struct)
         ->json json-> json-struct)

;; Association list for JSON structures;
;;  format: (listof Struct-Name Struct-Fields Field-Types Optional-Default) (all syntax objects)
(define-for-syntax struct-assoc (list))

;; (install-struct-assoc! name fields types opts?)
;; Installs a descriptor for the given (JSON) structure.
;; Arguments:
;;   name - Name (syntax object) of structure.
;;   fields - Fields (syntax object) of structure.
;;   types - Types (syntax object) of fields.
;;   opts? - Optional default values for fields (for structure).
(define-for-syntax (install-json-assoc! name fields types opts?)
  (set! struct-assoc
        (cons (list name fields types opts?) struct-assoc)))

;; (fetch-struct-assoc name)
;; Fetches the descriptor for a given (JSON) structure.
;; Arguments:
;;  name - Name (syntax object) of structure. 
;; Returns:
;;  (list Struct-Fields Field-Types Optional-Default) (all syntax objects)
(define-for-syntax (fetch-json-assoc name)
  (define res
    (filter
     (lambda (assoc)
       (free-identifier=? name (car assoc)))
     struct-assoc))
  (cond
    [(null? res)
     (raise-syntax-error #f
                         "Expected a JSON structure."
                         name)]
    [else
     (cdr (car res))]))

;; Helper functions for JSON contract errors.
(struct exn:fail:json-struct exn:fail:contract ())
(define-syntax (raise-json-error stx)
  (syntax-case stx (:)
    [(raise-json-error fmt args ...)
     #'(raise (exn:fail:json-struct
               (format fmt args ...)
               (current-continuation-marks)))]))

;; (+-> a b)
;; Generates the syntax identifier a->b
(define-for-syntax (+-> a b)
  (format-id (if (syntax? a) a b)
             "~a->~a"
             (if (syntax? a) (syntax-e a) a)
             (if (syntax? b) (syntax-e b) b)))

;; (check-type T x)
;; Returns x, ensuring that it is of type T.
;; Arguments:
;;   T - Type.
;;   x - Value.
(define-syntax (check-type stx)
  (syntax-case stx ()
    ((_ T x)
     #`(let
           ([T? (make-predicate T)])
         (if (T? x) x
             (raise-json-error "Expected (type) ~a, got ~s." '#,(syntax->datum #'T) x))))))

;; (->json T)
;; Returns a helper function for converting an object of type T to a JSExpr
;; Arguments:
;;  T - Type.
;; Returns:
;;  (-> T JSExpr)
(define-syntax (->json T)
  (syntax-case T (List Listof Integer Inexact-Real Boolean String Option null)
    [(_ Integer) #'(lambda ([x : Integer]) : JSExpr x)]
    [(_ Boolean) #'(lambda ([x : Boolean]) : JSExpr x)]
    [(_ String) #'(lambda ([x : String]) : JSExpr x)]
    [(_ Inexact-Real) #'(lambda ([x : Inexact-Real]) : JSExpr x)]
    [(_ 'null) #'(lambda ([x : 'null]) : JSExpr x)]
    [(_ (Option T)) #'(lambda ([x : (Option T)]) : JSExpr x)]
    [(_ (List T ...))
     (with-syntax
         ([(v ...) (generate-temporaries #'(T ...))])
       #`(lambda ([x : (List T ...)]) : JSExpr
           (match x
             [(list v ...)
              (list
               #,@(map
                   (lambda (type value)
                     #`((->json #,type) #,value))
                   (syntax->list #'(T ...))
                   (syntax->list #'(v ...))))])))]
    [(_ (Listof T1))
     #`(lambda ([l : (Listof T1)]) : JSExpr
         (map (->json T1) l))]
    [(_ T) (+-> #'T 'json)]))

;; (json-> T)
;; Returns a helper function for converting a JSExpr to an object of type T.
;; Arguments:
;;  T - Type.
;; Returns:
;;  (-> JSExpr T)
(define-syntax (json-> T)
  (syntax-case T (List Listof Integer Inexact-Real Boolean String Option null)
    [(_ Integer) #`(lambda ([x : JSExpr]) : Integer
                     (check-type Integer x))]
    [(_ Boolean) #`(lambda ([x : JSExpr]) : Boolean
                     (check-type Boolean x))]
    [(_ String) #`(lambda ([x : JSExpr]) : String
                    (check-type String x))]
    [(_ Inexact-Real) #`(lambda ([x : JSExpr]) : Inexact-Real
                          (check-type Inexact-Real x))]
    [(_ 'null) #`(lambda ([x : JSExpr]) : 'null
                  (check-type 'null x))]
    [(_ (Option T))
     #`(lambda ([x : JSExpr]) : (Option T)
         (if x ((json-> T) x) x))]
    [(_ (List T ...))
     #`(lambda ([x : JSExpr]) : (List T ...)
         (check-type (List T ...) x))]
    [(_ (Listof T))
     #`(lambda ([l : JSExpr]) : (Listof T)
         (map (json-> T)
              (cond
                [(list? l) l]
                [else (raise-json-error "Expected (list) ~a, got ~s." '#,(syntax->datum #'T) l)])))]
    [(_ T) (+-> 'json #'T)]))

;; (make->json name fields types [parent #f])
;; Generates name->json : (-> name JSExpr)
;;
;; Arguments:
;;  name - Name of structure.
;;  fields - Fields of structure.
;;  types - Types of fields.
;;  parent - Optional, parent (supertype) of structure.
(define-for-syntax (make->json name fields types [parent #f])
  #`(define #,(+-> name 'json)
      (lambda ([obj : #,name])
        : (HashTable Symbol JSExpr)
        (let
            ([assocs : (Listof (Pairof Symbol JSExpr))
                     (foldl
                      (lambda ([assoc : (U False (Pairof Symbol JSExpr))]
                               [assocs : (Listof (Pairof Symbol JSExpr))])
                        (if assoc (cons assoc assocs) assocs))
                      '()
                      (list
                       #,@(map
                           (lambda (field type)
                             #`(let ([field-value
                                      ((->json #,type)
                                       (#,(format-id name "~a-~a" name field) obj))])
                                 #,(syntax-case type (Option)
                                     [(Option T)
                                      #`(if field-value (cons '#,field field-value) #f)]
                                     [else #`(cons '#,field field-value)])))
                           (syntax->list fields)
                           (syntax->list types))))])
          #,(if parent
                #`(let ([parent : (HashTable Symbol JSExpr)
                                (#,(+-> parent 'json) obj)])
                    (make-immutable-hash
                     (append (hash->list parent) assocs)))
                #`(make-immutable-hash assocs))))))

;; (make-json-> name fields types opts?)
;; Generates json->name : (-> JSExpr Name)
;; Arguments:
;;  name - Name of structure.
;;  fields - Fields of structure.
;;  types - Types of fields.
;;  opts? - Optional default values.
(define-for-syntax (make-json-> name fields types opts?)
  #`(define #,(+-> 'json name)
      (lambda ([obj : JSExpr])
        (cond
          [(hash? obj)
           (#,name
            #,@(map
                (lambda (field type opt?)
                  (define opt-thunk
                    (syntax-case opt? ()
                      [(opt)
                       #`(lambda () : #,type opt)]
                      [(opt _ ...)
                       (raise-syntax-error #f
                                           "Expected only one expression for default value for field."
                                           name field)]
                      [(_ ...)
                       (syntax-case type (Option)
                         [(Option T)
                          #`(lambda () #f)]
                         [else
                          #`(lambda () (raise-json-error "json->~a: Missing field ~a in ~s." '#,(syntax->datum name) '#,field obj))])]))
                  #`(cond
                      [(hash-has-key? obj '#,field)
                       (define field-value (hash-ref obj '#,field))
                       (check-type #,type
                                   ((json-> #,type) field-value))]
                      [else
                       (#,opt-thunk)]))
                (syntax->list fields)
                (syntax->list types)
                (syntax->list opts?)))]
          [else
           (raise-json-error "json:->~a: Expected object, got ~s." '#,(syntax->datum name) obj)]))))

;; (json-struct name ([field : type opt?] ...) options ...)
;; (json-struct name parent ([field : type opt?] ...) options ...)
;;
;; Syntax for defining a structure with ->json and json-> operators.
;; Arguments:
;;  name - Name of structure.
;;  parent - Optional, JSON structure supertype.
;;  field, type - Field and type of field.
;;  opt? - Optional default initializer (for filling in missing values from JSON)
;;  options - Struct options, passed to (struct) form.
(define-syntax (json-struct stx)
  (syntax-case stx (:)
    [(_ name ([fields : types opts? ...] ...) opt ...)
     (begin
       #`(begin
           (begin-for-syntax
             (install-json-assoc! #'name #'(fields ...) #'(types ...) #'((opts? ...) ...)))
           (struct name ([fields : types] ...) opt ...)
           #,(make->json #'name #'(fields ...) #'(types ...))
           #,(make-json-> #'name #'(fields ...) #'(types ...) #'((opts? ...) ...))))]
    [(_ name
        parent
        ([fields : types opts? ...] ...) opt ...)
     (begin
       (define par-assoc (fetch-json-assoc #'parent))
       (define par-fields (car par-assoc))
       (define par-types (cadr par-assoc))
       (define par-opts? (caddr par-assoc))
       #`(begin
           (begin-for-syntax
             (install-json-assoc! #'name #'#,#`(#,@par-fields fields ...) #'#,#`(#,@par-types types ...) #'#,#`(#,@par-opts? (opts? ...) ...)))
           (struct name parent ([fields : types] ...) opt ...)
           #,(make->json #'name
                         #'(fields ...)
                         #'(types ...)
                         #'parent)
           #,(make-json-> #'name
                          #`(#,@par-fields fields ...)
                          #`(#,@par-types types ...)
                          #`(#,@par-opts? (opts? ...) ...))))]))

;(json-struct foo ([x : Integer 5] [y : (List String String)]) #:transparent)
;(json-struct bar foo ([z : Integer]) #:transparent)
;(json-struct faz bar ([a : String]) #:transparent)
;(foo->json (foo 10 15))
;(foo->json (foo 10 15))
;((json-> (Listof foo)) ((->json (Listof foo)) (list (foo 5 '("a" "b")))))
;((->json String) "args")
;((json-> (Listof Integer)) (list 10 15 20))
;(json->bar (bar->json (bar 6 (list "b" "c") 12)))
;(json->foo #{'#hash((y . ("a" "b"))) :: JSExpr})
;(json-struct baz ([z : (Option Integer)]) #:transparent)
;(baz->json (baz #f))
;(json->baz #{'#hash() :: JSExpr})
;(json-struct foobar ([q : Integer]))
;(json->foobar #{'#hash() :: JSExpr})
;(json-struct foo ([x : 'null]))
;(json->foo (foo->json (foo 'null)))
