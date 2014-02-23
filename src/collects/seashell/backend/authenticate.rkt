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
(require seashell/crypto
         seashell/log
         seashell/backend/project
         racket/serialize
         net/base64
         json)
(provide exn:authenticate? authenticate install-server-key! make-nonce
         make-authenticate-response make-download-token check-download-token)

;; Authentication error exception.
(struct exn:authenticate exn:fail ())

;; Current session key.
(define server-key (void))

;; Contracts
(define download-token/c (and/c jsexpr? (list/c string? string? string?)))
(define authenticate-token/c (and/c jsexpr? (list/c (listof byte?) (listof byte?) (listof byte?))))

;; (install-server-key! key) -> (void)
;; Installs the server key.
;;
;; Arguments:
;;  key - Secret shared key.
(define/contract (install-server-key! key)
  (-> bytes? void?)
  (set! server-key key))

;; (authenticate token expected) -> (void)
;; Attempts to authenticate given an authentication token.
;; Throws an exception if authentication fails.
;;
;; Arguments:
;;  token - Authentication token.
;;  expected - Expected decrypted bytestring.
;; Returns:
;;  (void) 
;; Raises exn:authenticate if an error occurred.
(define/contract (authenticate token expected)
  (-> authenticate-token/c bytes? void?)
  (match-define `(,iv ,coded ,tag) (map (curry apply bytes) token))
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

;; (make-authenticate-response plain) -> authenticate-token/c
;; Makes a response to an authentication challenge.
;;
;; Arguments:
;;  plain - Authentication challenge.
;; Returns:
;;  token - Authentication token satisfying the challenge.
(define/contract (make-authenticate-response plain)
  (-> bytes? authenticate-token/c)
  (define-values (iv coded tag) (seashell-encrypt server-key plain #""))
  (map bytes->list `(,iv ,coded ,tag)))

;; (make-nonce) -> nonce
;; Makes a nonce.
;; 
;; Returns:
;;  Bytestring that can be used as a nonce.
(define make-nonce seashell-crypt-make-token)

;; (make-download-token project-name?) -> (list/c bytes? bytes? bytes?)
;; Creates and encrypts a download token
;;
;; Arguments:
;;  project - Name of project.
;; Returns
;;  JSON expression representing the download token.
(define/contract (make-download-token project)
  (-> (and/c project-name? is-project?) download-token/c)
  (define-values
    (iv coded tag)
    (seashell-encrypt server-key
      (with-output-to-bytes (thunk (write (serialize (list project
        (+ 1000 (current-milliseconds)))))))
      #""))
  (map (compose bytes->string/utf-8 base64-encode) `(,iv ,coded ,tag)))

;; (check-download-token bytes? bytes? bytes?) -> bytes?
;;  Checks the validity of a download token
;;
;; Params:
;;  token - Token created from make-download-token.
;;
;; Returns:
;;  Project name
;;
;; Raises exn:authenticate if token is expired
;; Raises exn:project if project does not exist
(define/contract (check-download-token token)
  (-> download-token/c bytes?)
  (with-handlers
    ([exn:crypto?
       (lambda (exn) (raise (exn:authenticate "Authentication error!" (current-continuation-marks))))])
    (match-define `(,iv ,coded ,tag)
                  (map (compose base64-decode string->bytes/utf-8) token))
    (define decrypted-token (seashell-decrypt server-key iv tag coded #""))
    (define vals (with-input-from-bytes
      decrypted-token (thunk (deserialize (read)))))
    (if (and (project-name? (first vals))
             (is-project? (first vals)))
        (if (<= (current-milliseconds) (second vals))
            (first vals)
            (raise (exn:authenticate "Download token expired."
              (current-continuation-marks))))
        (raise (exn:project "Token for non-existent project."
          (current-continuation-marks))))))
