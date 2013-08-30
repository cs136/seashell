#lang racket/base
;; Seashell collection
;; Copyright (C) 2013 The Seashell Maintainers.
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
(require racket/contract)

(provide
  (contract-out
    [format-stack-trace
      (-> (listof (cons/c (or/c #f symbol?) (or/c #f srcloc?)))
          (listof string?))]))

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
                "<unknown location>"))))
