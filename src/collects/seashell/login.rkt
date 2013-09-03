#lang racket
;; Seashell's login service.
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
(require net/cgi
         json)

(define (binding-or-f key bdgs)
  (define ext (extract-bindings key bdgs))
  (if (empty? ext) #f (first ext)))

(define (response jsexpr)
  (printf "Content-type: text/html\n\n")
  (write-bytes (jsexpr->bytes jsexpr)))

(define (main)
  (define bindings (get-bindings))
  (define uname  (binding-or-f 'username bindings))
  (define passwd (binding-or-f 'password bindings))
  (response #hasheq((status . "ok"))))

(void (main))
