#lang racket 
;; Seashell's cryptographic communications backend.
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
(require ffi/unsafe
         ffi/unsafe/define)
(require racket/runtime-path)
(require seashell/seashell-config)
(require (prefix-in contract: racket/contract))
(require json)

(define-logger crypto)
(struct exn:crypto exn:fail:user ())

;; Load the crypto library
(define-ffi-definer define-crypto 
                    (ffi-lib (read-config 'seashell-crypto)))

;; Setup and Error functions
(define-crypto seashell_crypt_setup (_fun -> _int))
(define-crypto seashell_crypt_error (_fun -> _string))

;; Error handling function.
(define (check result function)
  (unless (zero? result)
    (raise (exn:crypto (format "~a: ~a" function (seashell_crypt_error))
                       (current-continuation-marks)))))

;; Encryption and Decryption routines.
(define-crypto seashell_encrypt
               (_fun _bytes ;; key[16]
                     _bytes ;; iv[12]
                     _bytes ;; *plain
                     _uint32 ;; plain_len
                     _bytes ;; auth
                     _uint32 ;; auth_len
                     _bytes ;; coded
                     _bytes ;; tag[16]
                     -> (r : _int)
                     -> (check r 'seashell_encrypt)))
(define-crypto seashell_decrypt
               (_fun _bytes ;; key[16]
                     _bytes ;; iv[12]
                     _bytes ;; *plain
                     _uint32 ;; plain_len
                     _bytes ;; auth
                     _uint32 ;; auth_len
                     _bytes ;; coded
                     _bytes ;; tag[16]
                     -> (r : _int)
                     -> (check r 'seashell_decrypt)))

;; IV and Key Generation functions.
(define (enough-bytes r n function)
  (unless (>= r n)
          (raise (exn:crypto (format "~a: Not enough random bytes!" function)
                             (current-continuation-marks)))))

(define-crypto seashell_make_iv
               (_fun _bytes ;; iv[12]
                     -> (r : _int)
                     -> (enough-bytes r 12 'seashell_make_iv)))
(define-crypto seashell_make_key
               (_fun _bytes ;; key[16]
                     -> (r : _int)
                     -> (enough-bytes r 12 'seashell_make_iv)))

;; (seashell-crypt-make-key)
;;
;; Returns a new AES-128 key.
(define/contract (seashell-crypt-make-key)
  (contract:-> bytes?)
  (define key (make-bytes 16))
  (seashell_make_key key)
  key)

;; (seashell-crypt-make-iv)
;; 
;; Returns a new AES-128-GCM IV [12 bytes long]
(define/contract (seashell-crypt-make-iv)
  (contract:-> bytes?)
  (define iv (make-bytes 12))
  (seashell_make_iv iv)
  iv)

;; (seashell-encrypt key frame plain) -> (values iv coded tag)
;; Encrypts frame (a sequence of bytes) using AES128-GCM
;; and authenticates (iv|plain).
;;
;; Arguments:
;;  key - 16-byte AES-128 key.
;;  frame - Frame to encrypt.
;;  plain - Plaintext authenticated data.
;;
;; Returns:
;;  (values iv coded tag) - IV and GCM tag respectively.
(define/contract (seashell-encrypt key frame plain)
  (contract:-> bytes? bytes? bytes? (values bytes? bytes? bytes?))
  (define tag (make-bytes 16))
  (define coded (make-bytes (bytes-length frame)))
  (define iv (seashell-crypt-make-iv))
  (define auth (bytes-append iv plain))
  (seashell_encrypt
    key iv
    frame (bytes-length frame)
    auth (bytes-length auth)
    coded tag)
  (values iv coded tag))

;; (seashell-decrypt key iv tag coded plain) -> frame
;; Decrypts coded into frame, and authenticates both coded
;; and plain against tag with AES128-GCM
;;
;; Arguments:
;;  key - 16-byte AES-128 key.
;;  coded - Frame to decrypt.
;;  plain - Authenticated plaintext.
;;
;; Returns:
;;  Decrypted frame.
(define/contract (seashell-decrypt key iv tag coded plain)
  (contract:-> bytes? bytes? bytes? bytes? bytes? bytes?)
  (define frame (make-bytes (bytes-length coded)))
  (define auth (bytes-append iv plain))
  (seashell_decrypt
    key iv
    frame (bytes-length frame)
    auth (bytes-length auth)
    coded tag)
  frame)

;; (seashell-crypt-key->client key) -> jsexpr?
;; Given an encryption key, represents it in a way that 
;; the client side code in JavaScript can easily handle.
;;
;; This code will produce a JSON expression that will be sent directly to 
;; the JavaScript.  Make sure this remains in sync with the
;; JavaScript implementation in crypto.js
;;
;; Everything else will be written out as raw bytes, and will be
;; expected to in future versions of this library.  Everything
;; else currently is IV, ciphertext, extra authenticated data,
;; GCM authentication tag, and plaintext.
(define/contract (seashell-crypt-key->client key)
  (contract:-> bytes? jsexpr?)
  ;; Currently, write out the key as a JSON
  ;; list of 4 4-byte big-endian signed words.
  (define keyA (integer-bytes->integer (subbytes key 0 4) #t #t))
  (define keyB (integer-bytes->integer (subbytes key 4 8) #t #t))
  (define keyC (integer-bytes->integer (subbytes key 8 12) #t #t))
  (define keyD (integer-bytes->integer (subbytes key 12 16) #t #t))
  ;; Write it out as a bytestring representing [A, B, C, D]
  `(,keyA ,keyB ,keyC ,keyD))

;; (seashell-crypt-key->server key) -> bytes?
;; Given an encryption key, represents it in a way that 
;; the server side code in Racket can easily handle.
;;
;; This code will produce a bytestring that will be sent directly to 
;; the Racket server.  Make sure this remains in sync with the
;; function below.
;;
;; Everything else will be written out as raw bytes, and will be
;; expected to in future versions of this library.  Everything
;; else currently is IV, ciphertext, extra authenticated data,
;; GCM authentication tag, and plaintext.
(define/contract (seashell-crypt-key->server key)
  (contract:-> bytes? bytes?)
  key)

;; (seashell-crypt-key-server-read port) -> bytes?
;; Reads an encryption key from port that is represented in a way
;; that the server siide code in Racket can easily handle.
;; 
;; See (seashell-crypt-key->server key)
(define/contract (seashell-crypt-key-server-read port)
  (contract:-> port? bytes?)
  (read-bytes 16 port))

(when (equal? (seashell_crypt_setup) 1)
      (raise (exn:crypto (format "Couldn't load library: ~a" 
                                 (seashell_crypt_error))
                         (current-continuation-marks))))

(provide seashell-encrypt seashell-decrypt
  seashell-crypt-make-key seashell-crypt-key->client
  seashell-crypt-key->server seashell-crypt-key-server-read
  exn:crypto?)
