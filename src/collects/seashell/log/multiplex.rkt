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
(require racket/async-channel
         racket/match
         racket/list
         racket/function
         racket/contract)

(provide
  (contract-out
    [make-multiplexer
      (-> thread?)]
    [mt-subscribe
      (-> thread? async-channel?)]
    [mt-send
      (-> thread? any/c void?)]
    [mt-receive
      (-> async-channel? any/c)]))

(define (multiplex-loop clients)
  (match (thread-receive)
    [(list 'subscribe (? thread? thd) (? channel? resp-ch))
      (let ((new-chan (make-async-channel #f)))
        (channel-put resp-ch new-chan)
        (multiplex-loop
        (cons (cons thd new-chan) clients)))]
    [(var next-msg)
      (multiplex-loop
      (filter-map
        (lambda(client)
          (if (thread-dead? (car client))
              #f
              (begin
                (async-channel-put (cdr client) next-msg)
                client)))
        clients))]))

(define (make-multiplexer)
  (thread (thunk (multiplex-loop '()))))

(define (mt-subscribe thd)
  (let ((resp-ch (make-channel)))
    (thread-send thd `(subscribe ,(current-thread) ,resp-ch))
    (channel-get resp-ch)))

(define mt-receive async-channel-get)

(define mt-send thread-send)
