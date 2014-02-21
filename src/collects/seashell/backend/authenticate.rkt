#lang racket
;; Seashell's backend server.
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
(require seashell/crypto seashell/log)
(provide exn:authenticate? authenticate install-server-key! make-nonce make-authenticate-response)

;; Authentication error exception.
(struct exn:authenticate exn:fail ())

;; Current session key.
(define server-key (void))

;; (install-server-key! key) -> (void)
;; Installs the server key.
;;
;; Arguments:
;;  key - Secret shared key.
(define/contract (install-server-key! key)
  (-> bytes? void?)
  (set! server-key key))

;; (authenticate iv coded tag expected) -> (void)
;; Attempts to authenticate given some encrypted data.
;; Throws an exception if authentication fails.
;;
;; Arguments:
;;  iv, coded, tag - Bytes representing the authentication request.
(define/contract (authenticate iv tag coded expected)
  (-> bytes? bytes? bytes? bytes? void?)
  (with-handlers
    ([exn:crypto?
       (lambda (exn) (raise (exn:authenticate "Authentication error!" (current-continuation-marks))))])
    (unless
      (equal?
        (seashell-decrypt
          server-key
          iv
          tag
          coded
          #"")
        expected)
      (raise (exn:authenticate "Authentication error!" (current-continuation-marks)))))
  (void))

;; (make-authenticate-response plain) -> iv coded tag
;; Makes a response to an authentication challenge.
;;
;; Arguments:
;;  plain - Authentication challenge.
;; Returns:
;;  iv, coded, tag - GCM coded data that represents the response.
(define/contract (make-authenticate-response plain)
  (-> bytes? (values bytes? bytes? bytes?))
  (seashell-encrypt server-key plain #""))

;; (make-nonce) -> nonce
;; Makes a nonce.
;; 
;; Returns:
;;  Bytestring that can be used as a nonce.
(define make-nonce seashell-crypt-make-token)
