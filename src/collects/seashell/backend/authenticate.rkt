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
         make-authenticate-response make-download-token check-download-token
         make-challenge make-file-upload-token check-upload-token)

;; Authentication error exception.
(struct exn:authenticate exn:fail ())

;; Current session key.
(define server-key (void))

;; Contracts.
(define upload-token/c (and/c jsexpr? (list/c (listof byte?)
                                              (listof byte?)
                                              (listof byte?))))
(define download-token/c (and/c jsexpr? (list/c (listof byte?)
                                                (listof byte?)
                                                (listof byte?))))
(define authenticate-token/c (and/c jsexpr?
                                    (list/c (listof byte?)
                                            (listof byte?)
                                            (listof byte?)
                                            (listof byte?))))
(define challenge/c (and/c jsexpr? (listof byte?)))

;; (make-nonce/challenge) -> nonce
;; Makes a nonce.
;; 
;; Returns:
;;  Bytestring that can be used as a nonce.
(define make-nonce seashell-crypt-make-token)

;; (make-challenge) -> challenge/c
;; Makes an authentication challenge.
;;
;; Returns:
;;  An authentication challenge.
(define/contract (make-challenge)
  (-> challenge/c)
  (bytes->list (seashell-crypt-make-token)))

;; (install-server-key! key) -> (void)
;; Installs the server key.
;;
;; Arguments:
;;  key - Secret shared key.
(define/contract (install-server-key! key)
  (-> bytes? void?)
  (set! server-key key))

;; (authenticate token challenge) -> (void)
;; Attempts to authenticate given an authentication token.
;; Throws an exception if authentication fails.
;;
;; Arguments:
;;  token - Authentication token.
;;  challenge - Challenge that the token is a response for.
;; Returns:
;;  (void) 
;; Raises exn:authenticate if an error occurred.
(define/contract (authenticate token challenge)
  (-> authenticate-token/c challenge/c void?)
  (match-define `(,iv ,coded ,tag ,nonce) (map (curry apply bytes) token))
  (define challenge-bytes (apply bytes challenge))
  
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
        (bytes-append nonce challenge-bytes))  
      (raise (exn:authenticate "Authentication error!" (current-continuation-marks)))))
  (void))

;; (make-authenticate-response plain) -> authenticate-token/c
;; Makes a response to an authentication challenge.
;;
;; Arguments:
;;  plain - Authentication challenge.
;; Returns:
;;  token - Authentication token satisfying the challenge.
(define/contract (make-authenticate-response challenge)
  (-> challenge/c authenticate-token/c)
  (define nonce (make-nonce))
  (define challenge-bytes (apply bytes challenge))
  (define-values (iv coded tag) (seashell-encrypt server-key
                                                  (bytes-append nonce challenge-bytes)
                                                  #""))
  (map bytes->list `(,iv ,coded ,tag ,nonce)))

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
        (+ 60000 (current-milliseconds)))))))
      #""))
  (map bytes->list `(,iv ,coded ,tag)))

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
  (-> download-token/c (and/c project-name? is-project?))
  (with-handlers
    ([exn:crypto?
       (lambda (exn) (raise (exn:authenticate "Authentication error!" (current-continuation-marks))))])
    (match-define `(,iv ,coded ,tag)
                  (map list->bytes token))
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

;; (make-file-upload-token project filename) -> iv coded tag
;; Creates a file upload token from a client request
;;
;; Arguments:
;;  project - name of the project to which the file is being uploaded
;;  filename - name of the file being uploaded
;; Returns:
;;  iv, coded, tag - GCM coded data that represents the upload token
(define/contract (make-file-upload-token project filename)
  (-> string? string? upload-token/c)
  (define validity (+ (current-milliseconds) 60000))
  (define-values (iv coded tag)
    (seashell-encrypt
      server-key
      (with-output-to-bytes (lambda () (write (serialize (list project filename validity)))))
      #""))
  (map bytes->list (list iv coded tag)))


;; (check-upload-token upload-token/c) -> (list/c project? string?)
;;  Checks the validity of a upload token
;;
;; Params:
;;  token - Token created from make-upload-token.
;;
;; Returns:
;;  Project name and filename
;;
;; Raises exn:authenticate if token is expired
;; Raises exn:project if project does not exist
(define/contract (check-upload-token token)
  (-> upload-token/c (list/c (and/c project-name? is-project?) string?))
  (with-handlers
    ([exn:crypto?
       (lambda (exn) (raise (exn:authenticate "Authentication error!" (current-continuation-marks))))])
    (match-define `(,iv ,coded ,tag)
                  (map list->bytes token))
    (define decrypted-token (seashell-decrypt server-key iv tag coded #""))
    (define vals (with-input-from-bytes
      decrypted-token (thunk (deserialize (read)))))
    (if (and (project-name? (first vals))
             (is-project? (first vals)))
        (if (<= (current-milliseconds) (third vals))
            (list (first vals) (second vals))
            (raise (exn:authenticate "Upload token expired."
              (current-continuation-marks))))
        (raise (exn:project "Token for non-existent project."
          (current-continuation-marks))))))
