#lang racket
;; Seashell's authentication and communications backend.
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
(require racket/path)
(require racket/runtime-path)
(require seashell/seashell-config
         seashell/log)

(provide
 tunnel?
 tunnel-launch
 tunnel-process
 tunnel-in
 tunnel-out
 exn:tunnel?)

(struct tunnel (process in out status-thread))
(struct exn:tunnel exn:fail:user ())

;; (tunnel-launch user password) -> tunnel?
;; Launches an instance of the Seashell Tunnel backend.
;;
;; Configuration options (see seashell-config.rkt):
;;  host - host to connect to.
;;
;; Expects:
;;  seashell-tunnel should live in the same directory as this file.
;;
;; Arguments:
;;  user - User name.
;;  password - Password.
;;
;; Returns:
;;  A tunnel? structure, which contains the subprocess?,
;;  and the I/O ports required to communicate with the process.
;;  Standard error is read by the status thread and sent to the logger.
;;
;;  The tunnel either will have been set up OR an exception
;;  would have been raised at the end of this function.
;;
;; Exceptions:
;;  exn:tunnel on tunnel error.
(define/contract (tunnel-launch user password)
  (-> string? string? tunnel?)

  ;; Launch the process
  (define-values (process in out error)
    (subprocess #f #f #f
                (read-config 'tunnel-binary)
                user
                (read-config 'host)))
  ;; And the logger thread
  (define status-thread
    (thread
     (lambda ()
       (let loop ()
         (define line (read-line error))
         (when (not (eq? line eof))
           (logf/sync 'warn "tunnel stderr (~a@~a): ~a" user (read-config 'host) line)
           (loop))
         ;; EOF received - die.
         (close-input-port error))
       )))

  ;; Set unbuffered mode for the output port, so nothing funny happens.
  (file-stream-buffer-mode out 'none)

  ;; Write out the authentication details
  (define password-bytes (bytes-append (string->bytes/utf-8 password) (bytes 0)))
  (define password-length (bytes-length password-bytes))

  ;; Format:
  ;;  length - 4 bytes, unsigned integer, LSB order
  ;;  method - 1 byte, 0 for password authentication.
  ;;  password - length bytes
  (write-bytes (integer->integer-bytes password-length 4 #f #f) out)
  (write-byte 0 out)
  (write-bytes password-bytes out)

  ;; Wait on the input port and see what happens.
  (define check (read-byte in))
  (cond
    ;; Case 1 - EOF read.  Make sure the process exited cleanly.
    [(and (eof-object? check)
          (begin
            (subprocess-wait process)
            (not (equal? 0 (subprocess-status process)))))
     (define message (format "Seashell tunnel died with status ~a" (subprocess-status process)))
     (logf 'warn "~a" message)
     (thread-wait status-thread)
     (raise (exn:tunnel message (current-continuation-marks)))]
    ;; Case 2 - Unexpected byte read.  Tunnel will always write ASCII 'O' to output
    ;; before starting two-way data processing.
    [(not (equal? check 79))
     (define message (format "Seashell tunnel wrote bad status byte ~a" check))
     (logf 'warn "~a" message)
     (subprocess-kill process #t)
     (thread-wait status-thread)
     (raise (exn:tunnel message (current-continuation-marks)))]
    ;; Case 3 - All good.
    [(equal? check 79)
     #t])

  ;; All good.
  (tunnel process in out status-thread))
