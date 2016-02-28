#lang typed/racket
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
(provide
  (struct-out tunnel)
  (struct-out exn:tunnel)
  tunnel-close
  Tunnel)

(struct tunnel ([process : Subprocess] [in : Input-Port] [out : Output-Port] [status-thread : Thread]
                [hostname : String]))
(struct exn:tunnel exn:fail:user ([status-code : (U False Exact-Nonnegative-Integer)]))
(define-type Tunnel tunnel)

;; (tunnel-close tunnel)
;; Closes a tunnel.
;;
;; Arguments:
;;  tunnel - Tunnel to close.
(: tunnel-close (-> tunnel Void))
(define (tunnel-close tunnel)
  (close-input-port (tunnel-in tunnel))
  (close-output-port (tunnel-out tunnel))
  (void))
