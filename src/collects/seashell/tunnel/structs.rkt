#lang racket/base
;; Seashell's authentication and communications backend.
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
(require racket/contract
         racket/port)
(provide
  (struct-out tunnel)
  (struct-out exn:tunnel)
  tunnel-close)

(struct tunnel (process in out status-thread hostname))
(struct exn:tunnel exn:fail:user (status-code))

;; (tunnel-close tunnel)
;; Closes a tunnel.
;;
;; Arguments:
;;  tunnel - Tunnel to close.
(define/contract (tunnel-close tunnel)
  (-> tunnel? void?)
  (close-input-port (tunnel-in tunnel))
  (close-output-port (tunnel-out tunnel)))
