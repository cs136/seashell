#lang racket
;; Seashell's websocket library.
;; Copyright (C) 2013-2014 The Seashell Maintainers.
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
;; handshake-solution key
;; Calculates the correct WebSocket handshake response to key.
;;
;; Arguments:
;;  key - Client's handshake.
;; Returns:
;;  Handshake response.
(require net/base64
         openssl/sha1)

(provide handshake-solution-server
         handshake-solution-ok?
         make-handshake)

(define/contract (make-handshake)
  (-> bytes?)
  (base64-encode
   (apply bytes (for/list ([i (in-range 16)]) (random 256))) #""))

(define/contract (handshake-solution-server key)
  (-> bytes? bytes?)
  (base64-encode
   (with-input-from-bytes
    (bytes-append key #"258EAFA5-E914-47DA-95CA-C5AB0DC85B11")
    (lambda() (sha1-bytes (current-input-port))))
   #""))

(define/contract (handshake-solution-ok? key result)
  (-> bytes? bytes? boolean?)
  (equal? result (handshake-solution-server key)))


