#lang racket
;; Seashell's login gateway.
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
(require net/cgi
          net/url
          seashell/seashell-config
          seashell/log
          seashell/format-trace)

(provide gateway-main)

(define (gateway-main)
  (define (setup-log regexp file)
    (printf "Logging messages matching '~a' to '~a'.~n" regexp file)
    (make-fs-logger regexp file)
    (void))

  (setup-log "^web-exn$" "seashell-error.log")
  (setup-log "^info$" "info.log")
  (setup-log "^warn$" "warn.log")
  (setup-log "^exception$" "exn.log")

  (define ss-exn-handler
    (lambda(e)
      (when (not (exn:break? e))
        (logf 'exception "~a:~n ~a"
              (exn-message e)
              (foldl string-append ""
                    (format-stack-trace
                      (continuation-mark-set->context
                      (exn-continuation-marks e))))))
      ((error-escape-handler))))

  (uncaught-exception-handler ss-exn-handler)
