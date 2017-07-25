#lang typed/racket
;; Seashell's SQLite3 + Dexie bindings.
;; Copyright (C) 2013-2015 The Seashell Maintainers.
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
(require typed/json)
(require typed/db)
(require typed/db/sqlite3)
(require "support.rkt")

(provide apply-modifications
         fold-create-and-update
         fold-update-and-update)

;; (apply-modifications object updates) -> newObject
;; Given an JSON object and a JSON object representing updates as {key: newValue} pairs
;; applies the updates and returns the resulting object as a string.
;;
;; Arguments:
;;  object - JSON object to update, as JSExpr | String.
;;  updates - Updates to apply, as either a JSExpr HashTable or a String.
(: apply-modifications (-> (U String JSExpr) (U String (HashTable Symbol JSExpr)) String))
(define (apply-modifications _object _updates)
  (define updates (assert (jsexpr-or-string->jsexpr _updates) hash?))
  (for/fold ([object (string-or-jsexpr->string _object)])
            ([(_key _value) (in-hash updates)])
    (define key-path (symbol->string _key))
    (define update (jsexpr->string _value))
    (set-by-key-path object key-path update)))

;; (fold-update-and-update first second) -> newUpdate
;; Given a create and an update on the same object, returns the CREATE change
;; that is the result of applying the update on the create.
;;
;; Arguments:
;;  first, second - Updates to apply, as JSExpr | String.
(: fold-create-and-update (-> (U String JSExpr) (U String (HashTable Symbol JSExpr)) String))
(define (fold-create-and-update object update)
  (apply-modifications object update))

;; (fold-update-and-update first second) -> newUpdate
;; Given two updates to apply on a JSON object, returns the update that is the result
;; of applying the first update followed by the second update.
;;
;; Arguments:
;;  first, second - Updates to apply, as JSExpr | String.
(: fold-update-and-update (-> (U String JSExpr) (U String JSExpr) String))
(define (fold-update-and-update _update-old _update-new)
  (define update-old (assert (jsexpr-or-string->jsexpr _update-old) hash?))
  (define update-new (assert (jsexpr-or-string->jsexpr _update-new) hash?))
  (jsexpr->string (for/fold ([new-change update-old])
                            ([(_key-new _value-new) (in-hash update-new)])
                    (define key-new (symbol->string _key-new))
                    (define-values (had-parent? updated-change)
                      (for/fold ([had-parent? : Boolean #f]
                                 [updated-change : (HashTable Symbol JSExpr) new-change])
                                ([(_key-old _) (in-hash update-old)])
                        (define key-old (symbol->string _key-old))
                        (cond
                          [(string-prefix? key-new (string-append key-old "."))
                           (define key-new-rest (substring key-new (add1 (string-length key-old))))
                           (values #t (hash-set updated-change
                                                _key-old
                                                (string->jsexpr (set-by-key-path
                                                                 (hash-ref updated-change _key-old)
                                                                 key-new-rest
                                                                 (jsexpr->string _value-new)))))]
                          [else
                           (values (or had-parent? #f) updated-change)])))
                    (define new-updated-change
                      (cond
                        [had-parent? updated-change]
                        [else
                         (hash-set updated-change _key-new _value-new)]))
                    (define finalized-updated-change
                      (for/fold ([finalized-updated-change : (HashTable Symbol JSExpr) new-updated-change])
                                ([(_key-old _) (in-hash update-old)])
                        (define key-old (symbol->string _key-old))
                        (cond
                          [(string-prefix? key-old (string-append key-new "."))
                           (hash-remove finalized-updated-change _key-old)]
                          [else finalized-updated-change])))
                    finalized-updated-change)))
