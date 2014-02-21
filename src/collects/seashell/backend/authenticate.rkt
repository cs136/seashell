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
(require seashell/crypto)
(provide exn:authenticate? authenticate)

;; Authentication error exception.
(struct exn:authenticate exn:fail ())

;; (authenticate key iv coded tag) -> (void)
;; Attempts to authenticate given some encrypted data.
;; Throws an exception if authentication fails.
;;
;; Arguments:
;;  key - Secret shared key.
;;  iv, coded, tag - Bytes representing the authentication request.
(define/contract (authenticate key iv coded tag)
  (-> bytes? bytes? bytes? bytes? void?)
  (with-handlers
    ([exn:crypto?
       (raise (exn:authenticate "Authentication error!" (current-continuation-marks)))])
    (seashell-decrypt
      key
      (apply bytes iv)
      (apply bytes tag)
      (apply bytes coded)
      #""))
  (void))
