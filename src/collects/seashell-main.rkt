#lang racket/base
;; Seashell.
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
(require seashell/login/login-gateway)
(require seashell/backend/server)
(require seashell/seashell-config)
(require racket/cmdline racket/match)

(begin
  (define mode (make-parameter 'version))
  (command-line
    #:usage-help "Seashell multi-tool binary."
    #:once-each
    [("-l" "--login") "Run the login tool." (mode 'login)]
    [("-s" "--server") "Run the server." (mode 'server)]
    [("-v" "--version") "Prints version information. [default]" (mode 'version)])
  (match (mode)
         ['version (printf "Seashell v~a multi-tool binary - built from ~a (~a).~n"
                           SEASHELL_VERSION SEASHELL_BRANCH SEASHELL_COMMIT)]
         ['login (gateway-main)]
         ['server (backend-main)])
  (void))
