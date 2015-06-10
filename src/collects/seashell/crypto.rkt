#lang racket/base
;; Seashell's cryptographic communications backend.
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
(require ffi/unsafe
         ffi/unsafe/define 
         ffi/unsafe/alloc
         openssl/libcrypto
         openssl/libssl
         openssl
         json)
(require (prefix-in contract: racket/contract))

(provide seashell-encrypt seashell-decrypt
  seashell-crypt-make-key seashell-crypt-key->client
  seashell-crypt-key->server seashell-crypt-key-server-read
  seashell-crypt-make-token
  exn:crypto?)

;; Types/Variables
(struct exn:crypto exn:fail:user ())
(define _EVP_CIPHER_CTX (_cpointer '_EVP_CIPHER_CTX))
(define _EVP_CIPHER (_cpointer '_EVP_CIPHER))
(define-ffi-definer define-crypto libcrypto
                    #:default-make-fail make-not-available)
(define-ffi-definer define-ssl libssl
                    #:default-make-fail make-not-available)

;; Constants
(define EVP_CTRL_GCM_GET_TAG 16) 
(define EVP_CTRL_GCM_SET_TAG 17) 

;; Error Handling Functions
(define-crypto ERR_get_error (_fun -> _long))
(define-crypto ERR_peek_error (_fun -> _long))
(define-crypto ERR_error_string_n (_fun _long _bytes _long -> _void))
(define (get-error-message id)
  (let* ([buffer (make-bytes 512)])
    (ERR_error_string_n id buffer (bytes-length buffer))
    (regexp-match #rx#"^[^\0]*" buffer)))
(define (check result function [predicate? (lambda (v) (= v 1))])
  (unless (predicate? result)
    (raise (exn:crypto (format "~a: ~a" function (get-error-message (ERR_get_error)))
                       (current-continuation-marks)))))

;; Functions
(define-crypto EVP_EncryptInit_ex (_fun _EVP_CIPHER_CTX _EVP_CIPHER _pointer _bytes _bytes -> (r : _int)
                                        -> (check r 'EVP_EncryptInit_ex)))
(define-crypto EVP_DecryptInit_ex (_fun _EVP_CIPHER_CTX _EVP_CIPHER _pointer _bytes _bytes -> (r : _int)
                                        -> (check r 'EVP_DecryptInit_ex)))
(define-crypto EVP_aes_128_gcm (_fun -> _EVP_CIPHER))
(define-crypto EVP_EncryptUpdate (_fun _EVP_CIPHER_CTX _bytes (olen : (_ptr o _uint)) _bytes _int -> (r : _int)
                                       -> (begin (check r 'EVP_EncryptUpdate) olen)))
(define-crypto EVP_DecryptUpdate (_fun _EVP_CIPHER_CTX _bytes (olen : (_ptr o _uint)) _bytes _int -> (r : _int)
                                       -> (begin (check r 'EVP_DecryptUpdate) olen)))
(define-crypto EVP_EncryptFinal_ex (_fun _EVP_CIPHER_CTX _bytes (olen : (_ptr o _uint)) -> (r : _int)
                                        -> (begin (check r 'EVP_EncryptFinal_ex) olen)))
(define-crypto EVP_DecryptFinal_ex (_fun _EVP_CIPHER_CTX _bytes (olen : (_ptr o _uint)) -> (r : _int)
                                        -> (begin (check r 'EVP_DecryptFinal_ex) olen)))
(define-crypto EVP_CIPHER_CTX_free (_fun _EVP_CIPHER_CTX -> _void) #:wrap (deallocator))
(define-crypto EVP_CIPHER_CTX_new (_fun -> _EVP_CIPHER_CTX) #:wrap (allocator EVP_CIPHER_CTX_free))
(define-crypto EVP_CIPHER_CTX_ctrl (_fun _EVP_CIPHER_CTX _int _int _pointer -> (r : _int)
                                         -> (check r 'EVP_CIPHER_CTX_ctrl)))
(define-crypto RAND_bytes (_fun _bytes _int -> (r : _int) -> (check r 'RAND_bytes)))

;; encrypt_aes128_gcm (key:16) (iv:12) text (aad/optional) -> (values encrypted (tag:16))
;;
;; Encrypts and Authenticates.
(contract:define/contract (encrypt_aes128_gcm key iv text [aad #f])
  (contract:->* (bytes? bytes? bytes?) ((contract:or/c bytes? #f)) (values bytes? bytes?))
  (define CTX (EVP_CIPHER_CTX_new))
  (EVP_EncryptInit_ex CTX (EVP_aes_128_gcm) #f key iv)
  (when aad
    (EVP_EncryptUpdate CTX #f aad (bytes-length aad)))
  
  (define buffer (make-bytes (+ 128 (bytes-length text))))
  (define tag (make-bytes 16))

  (define first-len (EVP_EncryptUpdate CTX buffer text (bytes-length text)))
  (define second-len (EVP_EncryptFinal_ex CTX (cast (ptr-add buffer first-len) _pointer _bytes)))

  (EVP_CIPHER_CTX_ctrl CTX EVP_CTRL_GCM_GET_TAG 16 tag)

  (values (subbytes buffer 0 (+ first-len second-len)) tag))
;; decrypt_aes128_gcm (key:16) (iv:12) (tag:16) encrypted (aad/optional) -> text
;;
;; Decrypts and verifies.
(contract:define/contract (decrypt_aes128_gcm key iv tag text [aad #f])
  (contract:->* (bytes? bytes? bytes? bytes?) ((contract:or/c bytes? #f)) bytes?)
  (define CTX (EVP_CIPHER_CTX_new))
  (EVP_DecryptInit_ex CTX (EVP_aes_128_gcm) #f key iv)
  (EVP_CIPHER_CTX_ctrl CTX EVP_CTRL_GCM_SET_TAG 16 tag)

  (when aad
    (EVP_DecryptUpdate CTX #f aad (bytes-length aad)))
  
  (define buffer (make-bytes (+ 128 (bytes-length text))))

  (define first-len (EVP_DecryptUpdate CTX buffer text (bytes-length text)))
  (define second-len (EVP_DecryptFinal_ex CTX (cast (ptr-add buffer first-len) _pointer _bytes)))

  (subbytes buffer 0 (+ first-len second-len)))

;; (seashell-crypt-make-key)
;;
;; Returns a new AES-128 key.
(contract:define/contract (seashell-crypt-make-key)
  (contract:-> bytes?)
  (define key (make-bytes 16))
  (RAND_bytes key 16)
  key)

;; (seashell-crypt-make-iv)
;; 
;; Returns a new AES-128-GCM IV [12 bytes long]
(contract:define/contract (seashell-crypt-make-iv)
  (contract:-> bytes?)
  (define iv (make-bytes 12))
  (RAND_bytes iv 12)
  iv)

;; (seashell-crypt-make-token)
;; 
;; Returns a new token [32 bytes long]
(contract:define/contract (seashell-crypt-make-token)
  (contract:-> bytes?)
  (define token (make-bytes 32))
  (RAND_bytes token 32)
  token)

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
(contract:define/contract (seashell-encrypt key frame plain)
  (contract:-> bytes? bytes? bytes? (values bytes? bytes? bytes?))
  (define iv (seashell-crypt-make-iv))
  (define auth (bytes-append iv plain))
  (define-values (coded tag) (encrypt_aes128_gcm key iv frame auth))
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
(contract:define/contract (seashell-decrypt key iv tag coded plain)
  (contract:-> bytes? bytes? bytes? bytes? bytes? bytes?)
  (define auth (bytes-append iv plain))
  (decrypt_aes128_gcm key iv tag coded auth))

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
(contract:define/contract (seashell-crypt-key->client key)
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
(contract:define/contract (seashell-crypt-key->server key)
  (contract:-> bytes? bytes?)
  key)

;; (seashell-crypt-key-server-read port) -> bytes?
;; Reads an encryption key from port that is represented in a way
;; that the server siide code in Racket can easily handle.
;; 
;; See (seashell-crypt-key->server key)
(contract:define/contract (seashell-crypt-key-server-read port)
  (contract:-> port? bytes?)
  (read-bytes 16 port))

