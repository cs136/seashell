#lang typed/racket
;; Seashell
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
(provide list-diff list-lcs
         vector-diff vector-lcs)

;; 2D Array Helpers
(: offset (-> Nonnegative-Integer Nonnegative-Integer Nonnegative-Integer Nonnegative-Integer 
              Nonnegative-Integer))
(define (offset M N i j)
  (+ j (* i M)))
(: make-2d-vector (All (a) (-> Nonnegative-Integer Nonnegative-Integer a (Vectorof a))))
(define (make-2d-vector M N a)
  (make-vector (* M N) a))

;; (vector-lcs-C v1 v2)
;; Computes the dynamic programming table for LCS(v1, v2)
;;
;; Arguments:
;;  v1, v2 - Input vectors
(: vector-lcs-C
   (All (a) (-> (Vectorof a) (Vectorof a) (Vectorof Integer))))
(define (vector-lcs-C v1 v2)
  (define M (add1 (vector-length v1)))
  (define N (add1 (vector-length v2)))
  (define C (make-2d-vector M N 0))
  (for ([i : Nonnegative-Integer (in-range (sub1 M))])
    (for ([j : Nonnegative-Integer (in-range (sub1 N))])
      (cond
        [(equal? (vector-ref v1 i)
                 (vector-ref v2 j))
         (vector-set! C
                      (offset M N (add1 i) (add1 j))
                      (add1 (vector-ref C (offset M N i j))))]
        [else
         (vector-set! C
                      (offset M N (add1 i) (add1 j))
                      (max (vector-ref C (offset M N (add1 i) j))
                           (vector-ref C (offset M N i (add1 j)))))])))
  C)

;; (list/vector-lcs v1 v2)
;; Computes the LCS of v1, v2
;;
;; Arguments:
;;  v1, v2 - Input vectors
(: vector-lcs
   (All (a) (-> (Vectorof a) (Vectorof a) (Listof a))))
(define (vector-lcs v1 v2)
  (: vector-lcs-helper
     (All (a) (-> (Vectorof a) (Vectorof a) (Listof a))))
  (define (vector-lcs-helper v1 v2)
    (define C (vector-lcs-C v1 v2))
    (define M (add1 (vector-length v1)))
    (define N (add1 (vector-length v2)))
    
    (: backtrack
       (-> Integer Integer (Listof a)))
    (define (backtrack i j)
      (cond
        [(< i 0) '()]
        [(< j 0) '()]
        [(equal? (vector-ref v1 i) (vector-ref v2 j))
         (cons (vector-ref v1 i) (backtrack (sub1 i) (sub1 j)))]
        [(> (vector-ref C (offset M N (add1 i) j))
            (vector-ref C (offset M N i (add1 j))))
         (backtrack i (sub1 j))]
        [else
         (backtrack (sub1 i) j)]))
    (reverse (backtrack (- M 2) (- N 2))))
  ;; Do the stupid thing eliminating common things at the head/tail
  (define L1 (vector-length v1))
  (define L2 (vector-length v2))
  (define L (min L1 L2))
  ;; Find end of common head
  (: find-first (-> Integer))
  (define (find-first)
    (: find-first-helper (-> Integer Integer))
    (define (find-first-helper i)
      (cond
        [(>= i L) L]
        [(equal? (vector-ref v1 i) (vector-ref v2 i))
         (find-first-helper (add1 i))]
        [else i]))
    (find-first-helper 0))
  (define i (find-first))
  ;; Find start of common tail
  (: find-last (-> Integer))
  (define (find-last)
    (: find-last-helper (-> Integer Integer))
    (define (find-last-helper j)
      (cond
        [(>= j L) L]
        [(equal? (vector-ref v1 (- L1 1 j)) (vector-ref v2 (- L2 1 j)))
         (find-last-helper (add1 j))]
        [else j]))
    (find-last-helper 0))
  (define j0 (find-last))
  ;; Correct j0 if it overlaps with i
  (define j (min (- L1 i) (- L2 i) j0))
  ;; Construct LCS
  (append (vector->list (vector-take v1 i))
          (vector-lcs-helper (vector-drop (vector-drop-right v1 j) i)
                             (vector-drop (vector-drop-right v2 j) i))
          (vector->list (vector-take-right v1 j))))
(: list-lcs
   (All (a) (-> (Listof a) (Listof a) (Listof a))))
(define (list-lcs l1 l2)
  (vector-lcs (list->vector l1) (list->vector l2)))

;; (list/vector-diff v1 v2)
;; Computes the difference of v1, v2
;;
;; Arguments:
;;  v1, v2 - Input vectors
(: vector-diff
   (All (a) (-> (Vectorof a) (Vectorof a) (Listof (U a (List Boolean a))))))
(define (vector-diff v1 v2)
  (define lcs (vector-lcs v1 v2))
  
  (: helper
     (-> (Listof a) (Listof a) (Listof a) (Listof (U a (List Boolean a)))))
  (define (helper l1 l2 lcs)
    (cond
      [(and (null? l1) (null? l2) '())]
      [(null? l1)
       (cons (list #t (car l2)) (helper l1 (cdr l2) lcs))]
      [(null? l2)
       (cons (list #f (car l1)) (helper (cdr l1) l2 lcs))]
      [(and (equal? (car l1) (car lcs))
            (equal? (car l1) (car l2)))
       (cons (car l1) (helper (cdr l1) (cdr l2) (cdr lcs)))]
      [(equal? (car l1) (car lcs))
       (cons (list #t (car l2)) (helper l1 (cdr l2) lcs))]
      [else
       (cons (list #f (car l1)) (helper (cdr l1) l2 lcs))]))
  (helper (vector->list v1) (vector->list v2) lcs))
(: list-diff
   (All (a) (-> (Listof a) (Listof a) (Listof (U a (List Boolean a))))))
(define (list-diff l1 l2)
  (vector-diff (list->vector l1) (list->vector l2)))
