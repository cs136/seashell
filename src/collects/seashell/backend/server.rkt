#lang racket
;; Seashell's backend server.
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
(require seashell/crypto)
(require json)

;; Read the negotiated key from the login server, which is
;; provided to us on standard input.
(define key (seashell-crypt-key-server-read (current-input-port)))


(define/contract (handle-message message)
  (-> jsexpr? jsexpr?)
  (match message
         [`(hash-table
             (id ,id)
             (type "runProgram")
             (name ,name))
           (hash-table `(id ,id) `(result "unimplemented"))]
         [`(hash-table
             (id ,id)
             (type "compileProgram")
             (name ,name))
           (hash-table `(id ,id) `(result "unimplemented"))]
         [`(hash-table
             (id ,id)
             (type "getListing")

             
  )

(define (main-loop connection state)
  ;; TODO - probably want to sync here also on a CLOSE frame.
  (define encrypted-frame (seashell-websocket-receive connection))
  (match-define (opcode data)
    (struct seashell-websocket-frame _ _ opcode data))
  ;; Framing format (all binary bytes):
  ;; [IV - 12 bytes][GCM tag - 16 bytes][1 byte - Auth Len][Auth Plain][Encrypted Frame]
  (define iv (subbytes data 0 12))
  (define tag (subbytes data 12 28))
  (define authlen (bytes-ref data 28))
  (define auth (subbytes data 29 (+ 29 authlen)))
  (define encrypted (subbytes data (+ 29 authlen)))
  (define plain (seashell-decrypt key iv tag encrypted auth))
  ;; Parse plain as a JSON message. 
  (define message (bytes->jsexpr plain))
  ;; TODO handle id field in JSON object.
  (match message
    [`(hash-table
       ('type "runProgram")
       ('name ,name))]
    [`(hash-table
       ('type "compileProgram")
       ('name ,name))]
    [`(hash-table 
       ('type "getListing")
       ('project ,project))]
    [`(hash-table 
       ('type "loadFile")
       ('project ,project)
       ('name ,name))]
    [`(hash-table
       ('type "saveFile")
       ('project ,project)
       ('name ,name)
       ('contents ,contents))]
    [`(hash-table
       ('type "revertFile")
       ('project ,project)
       ('name ,name))])
  ;; Do some output
  (main-loop)
)
