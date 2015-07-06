#lang racket/base
;;; diff.rkt
;;; Copyright (c) 2011 M. Douglas Williams
;;;
;;; This program is free software: you can redistribute it and/or modify it under
;;; the terms of the GNU Lesser General Public License as published by the Free
;;; Software Foundation, either version 3 of the License, or (at your option) any
;;; later version.
;;;
;;; This program is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;; FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
;;; details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; This package provides a simple diff-like capability in Packet. This includes
;;; diffs of arbitrary lists or of text files. It also includes an implementation
;;; of the longest common subsequence (LCS) algorithm, which is the basis of the
;;; diff algorithm.

;;; This module requires the list-index function from SRFI 1: List Library.

(require (only-in srfi/1 list-index)
         racket/list
         racket/port
         racket/contract)

;;; Longest Common Subsequence

;;; The longest common subsequence (LCS) is the longest subsequence that is
;;; common to a set of sequences. We are solving the special case with exactly
;;; two sequences, which are represented as lists. This is the basis of the diff
;;; algorithm.

;;; The recursive longest common subsequence algorithm is given below. However,
;;; it has time complexity approaching 2^n.

;;; (define (list-lcs list-1 list-2 (test equal?))
;;;   (cond ((null? list-1) '())
;;;         ((null? list-2) '())
;;;         ((test (car list-1) (car list-2))
;;;          (cons (car list-1) (list-lcs (cdr list-1) (cdr list-2) test)))
;;;         (else
;;;          (let ((lcs-1 (list-lcs list-1 (cdr list-2) test))
;;;                (lcs-2 (list-lcs (cdr list-1) list-2 test)))
;;;            (if (> (length lcs-1) (length lcs-2))
;;;                lcs-1
;;;                lcs-2)))))

;;; The problem with the naive recursive solution is that the same subproblems
;;; are solved many different times. The solution is to use dynamic programming
;;; to check to see if we've solved a subproblem before. If we have, we use the
;;; previously computed solution. If not, we compute and store the solution. This
;;; is done via memoization.

;;; We could use David Herman's memoize package on PLaneT. I tried it and it
;;; works fine, but there are two issues that made me revert to 'rolling my own'
;;; memoization in this case.
;;;  1) The tests in Dave's PLaneT package pull in a lot of other packages and
;;;     many of them have not been converted to Racket. This results in many
;;;     error messages that I would rather not have users subjected to. [Although
;;;     they don't actually affect the memoization macros.]
;;;  2) In the case of the longest common subsequence algorithm, problem
;;;     instances are unique and we want a new cache for every problem instance.

;;; (list-lcs list-1 list-2 [test]) -> list?
;;;   list-1 : list?
;;;   list-2 : list?
;;;   test : (-> any/c any/c boolean?) = equal?
;;; Returns the longest common subsequence (LCS) of list-1 and list-2 using test
;;; to compare the elements. This is a memoized version of the recursive
;;; algorithm above. Note that the memoization cache is implemented as a two level
;;; index.
(define (list-lcs list-1 list-2 (test equal?))
  (let ((cache (make-hash)))
    (define (do-list-lcs list-1 list-2)
      (hash-ref!
       (hash-ref! cache list-1 (make-hash))
       list-2
       (lambda ()
         (cond ((null? list-1) '())
               ((null? list-2) '())
               ((test (car list-1) (car list-2))
                (cons (car list-1) (do-list-lcs (cdr list-1) (cdr list-2))))
               (else
                (let ((lcs-1 (do-list-lcs list-1 (cdr list-2)))
                      (lcs-2 (do-list-lcs (cdr list-1) list-2)))
                  (if (> (length lcs-1) (length lcs-2))
                      lcs-1
                      lcs-2)))))))
    (do-list-lcs list-1 list-2)))

;;; Example:

;;; (list-lcs '(a b c d f g h j q z) '(a b c d e f g i j k r x y z) eq?) =>
;;; (a b c d f g j z)

;;; Diff

;;; The diff algorithm computes the differences between two sequences in the form
;;; of what changes are required to the first sequence to produce the second
;;; sequence. This is based on the longest common subsequence.

;;; Once the longest common subsequence is computed (using list-lcs), it's easy
;;; to produce the diff-like output:
;;;   If an item is absent in the LCS but present in the first sequence, it must
;;;   have been deleted.
;;;   If an item is absent in the LCS but present in the second sequence, it must
;;;   have been added.

;;; (list-diff list-1 list-2 [test]) -> list?
;;;   list-1 : list?
;;;   list-2 : list?
;;;   test : (-> any/c any/c boolean?) = equal?
;;; Returns a list of the differences between list-1 and list-2 using test to
;;; compare the elements. Items common to both lists are incuded in the list as
;;; is. Items that have been added are included as a list whose first element is
;;; #:added followed by the added elements. Items that have been removed are
;;; included as a list whose first element is #:removed followed by the deleted
;;; elements.
(define (list-diff list-1 list-2 (test equal?))
  (let ((lcs (list-lcs list-1 list-2 test))
        (result '()))
    (for ((item (in-list lcs)))
      (let* ((sync-list-1 (list-index (lambda (x) (test x item)) list-1))
             (sync-list-2 (list-index (lambda (x) (test x item)) list-2))
             (removed (take list-1 sync-list-1))
             (added (take list-2 sync-list-2)))
        (set! list-1 (drop list-1 (add1 sync-list-1)))
        (set! list-2 (drop list-2 (add1 sync-list-2)))
        (when (not (null? removed))
          (set! result (append result (list (cons #f removed)))))
        (when (not (null? added))
          (set! result (append result (list (cons #t added)))))
        (set! result (append result (list item)))))
    (when (not (null? list-1))
      (set! result (append result (list (cons #f list-1)))))
    (when (not (null? list-2))
      (set! result (append result (list (cons #t list-2)))))
    result))

;;; Example:
;;; (list-diff '(a b c d f g h j q z) '(a b c d e f g i j k r x y z) eq?) =>
;;; (a b c d (#:added e) f g (#:removed h) (#:added i) j (#:removed q) (#:added k r x y) z)

;;; Example:
;;; (list-diff
;;;  '("This part of the"
;;;    "document has stayed the"
;;;    "same from version to"
;;;    "version.  It shouldn't"
;;;    "be shown if it doesn't"
;;;    "change.  Otherwise, that"
;;;    "would not be helping to"
;;;    "compress the size of the"
;;;    "changes."
;;;    ""
;;;    "This paragraph contains"
;;;    "text that is outdated."
;;;    "It will be deleted in the"
;;;    "near future."
;;;    ""
;;;    "It is important to spell"
;;;    "check this dokument. On"
;;;    "the other hand, a"
;;;    "misspelled word isn't"
;;;    "the end of the world."
;;;    "Nothing in the rest of"
;;;    "this paragraph needs to"
;;;    "be changed. Things can"
;;;    "be added after it.")
;;;  '("This is an important"
;;;    "notice! It should"
;;;    "therefore be located at"
;;;    "the beginning of this"
;;;    "document!"
;;;    ""
;;;    "This part of the"
;;;    "document has stayed the"
;;;    "same from version to"
;;;    "version.  It shouldn't"
;;;    "be shown if it doesn't"
;;;    "change.  Otherwise, that"
;;;    "would not be helping to"
;;;    "compress anything."
;;;    ""
;;;    "It is important to spell"
;;;    "check this document. On"
;;;    "the other hand, a"
;;;    "misspelled word isn't"
;;;    "the end of the world."
;;;    "Nothing in the rest of"
;;;    "this paragraph needs to"
;;;    "be changed. Things can"
;;;    "be added after it."
;;;    ""
;;;    "This paragraph contains"
;;;    "important new additions"
;;;    "to this document.")
;;;  string=?) =>
;;; ((#:added "This is an important" 
;;;           "notice! It should"
;;;           "therefore be located at"
;;;           "the beginning of this"
;;;           "document!"
;;;           "")
;;;   "This part of the"
;;;   "document has stayed the"
;;;   "same from version to"
;;;   "version.  It shouldn't"
;;;   "be shown if it doesn't"
;;;   "change.  Otherwise, that"
;;;   "would not be helping to"
;;;   (#:removed "compress the size of the"
;;;              "changes.")
;;;   (#:added "compress anything.")
;;;   ""
;;;   (#:removed "This paragraph contains"
;;;              "text that is outdated."
;;;              "It will be deleted in the"
;;;              "near future." "")
;;;   "It is important to spell"
;;;   (#:removed "check this dokument. On")
;;;   (#:added "check this document. On")
;;;   "the other hand, a"
;;;   "misspelled word isn't"
;;;   "the end of the world."
;;;   "Nothing in the rest of"
;;;   "this paragraph needs to"
;;;   "be changed. Things can"
;;;   "be added after it."
;;;   (#:added ""
;;;            "This paragraph contains"
;;;            "important new additions"
;;;            "to this document."))

;;; Text File Diff

;;; A common use of diff is to print the differences between two text file -
;;; like the Unix diff command. The files are read as sequences of lines and
;;; list-diff is used to compute the differences between the files. The
;;; differences are then printed (and void returned). Lines common to both files
;;; are denoted by "=|", lines that are added are denoted by ">|", and lines that
;;; are removed are denoted by "<|".
;;;
;;; At some point it might me nice to change this to print in a format similar to
;;; the Unix diff command format.

;;; (ile-diff file-1 file-2) -> void?
;;;   file-1 : path-string?
;;;   file-2 : path-string?
;;; Prints the differences between file-1 and file-2.
(define (file-diff file-1 file-2 )
  (let* ((port-1 (open-input-file file-1 #:mode 'text))
         (port-2 (open-input-file file-2 #:mode 'text))
         (diffs (list-diff (port->lines port-1) (port->lines port-2) string=?)))
    (for ((diff (in-list diffs)))
      (cond ((and (list? diff) (eq? (car diff) #t))
             (for ((added (in-list (cdr diff))))
               (printf ">|~a~n" added)))
            ((and (list? diff) (eq? (car diff) #f))
             (for ((added (in-list (cdr diff))))
               (printf "<|~a~n" added)))
            (else
             (printf "=|~a~n" diff))))))

;;; Example:
;;; Print the differences between two text files.
;;;
;;; original.txt:
;;; This part of the
;;; document has stayed the
;;; same from version to
;;; version.  It shouldn't
;;; be shown if it doesn't
;;; change.  Otherwise, that
;;; would not be helping to
;;; compress the size of the
;;; changes.
;;; 
;;; This paragraph contains
;;; text that is outdated.
;;; It will be deleted in the
;;; near future.
;;; 
;;; It is important to spell
;;; check this dokument. On
;;; the other hand, a
;;; misspelled word isn't
;;; the end of the world.
;;; Nothing in the rest of
;;; this paragraph needs to
;;; be changed. Things can
;;; be added after it.
;;;
;;; new.txt:
;;; This is an important
;;; notice! It should
;;; therefore be located at
;;; the beginning of this
;;; document!
;;; 
;;; This part of the
;;; document has stayed the
;;; same from version to
;;; version.  It shouldn't
;;; be shown if it doesn't
;;; change.  Otherwise, that
;;; would not be helping to
;;; compress anything.
;;; 
;;; It is important to spell
;;; check this document. On
;;; the other hand, a
;;; misspelled word isn't
;;; the end of the world.
;;; Nothing in the rest of
;;; this paragraph needs to
;;; be changed. Things can
;;; be added after it.
;;; 
;;; This paragraph contains
;;; important new additions
;;; to this document.
;;;
;;; (file-diff "original.txt" "new.txt")
;;; >|This is an important
;;; >|notice! It should
;;; >|therefore be located at
;;; >|the beginning of this
;;; >|document!
;;; >|
;;; =|This part of the
;;; =|document has stayed the
;;; =|same from version to
;;; =|version.  It shouldn't
;;; =|be shown if it doesn't
;;; =|change.  Otherwise, that
;;; =|would not be helping to
;;; <|compress the size of the
;;; <|changes.
;;; >|compress anything.
;;; =|
;;; <|This paragraph contains
;;; <|text that is outdated.
;;; <|It will be deleted in the
;;; <|near future.
;;; <|
;;; =|It is important to spell
;;; <|check this dokument. On
;;; >|check this document. On
;;; =|the other hand, a
;;; =|misspelled word isn't
;;; =|the end of the world.
;;; =|Nothing in the rest of
;;; =|this paragraph needs to
;;; =|be changed. Things can
;;; =|be added after it.
;;; >|
;;; >|This paragraph contains
;;; >|important new additions
;;; >|to this document.

;;; References
;;;
;;;  1) "Longest common subsequence problem", Wikipedia, The Free Encyclopedia, 
;;;     http://en.wikipedia.org/wiki/Longest_common_subsequence_problem
;;;     (Accessed January 13, 2011).
;;;  2) "Diff", Wikipedia, The Free Encyclopedia,
;;;     http://en.wikipedia.org/wiki/Diff (Accessed January 13, 2011).
;;;  3) "Algorithm Implementation/Strings/Longest common subsequence", WikiBooks,
;;;     Open books for an open world,
;;;     http://en.wikibooks.org/wiki/Algorithm_implementation/Strings/Longest_common_subsequence
;;;     (Accessed January 13, 2011).

;;; Module Contracts

(provide/contract
 (list-lcs
  (->* (list? list?)
       ((-> any/c any/c boolean?))
       list?))
 (list-diff
  (->* (list? list?)
       ((-> any/c any/c boolean?))
       list?))
 (file-diff
  (-> path-string? path-string? void?)))
