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
(require seashell/tunnel/structs
         (prefix-in password: seashell/tunnel/password-tunnel)
         (prefix-in uw: seashell/tunnel/uw-tunnel))
(provide
  tunnel?
  Tunnel
  (struct-out exn:tunnel)
  tunnel-close
  password:tunnel-launch
  uw:tunnel-launch
  tunnel-process
  tunnel-in
  tunnel-out
  tunnel-hostname)
