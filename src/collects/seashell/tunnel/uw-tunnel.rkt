#lang racket
;; Seashell's authentication and communications backend.
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
(require racket/path)
(require racket/runtime-path)
(require seashell/seashell-config
         seashell/log
         seashell/tunnel/structs)

(provide tunnel-launch)
;; (tunnel-launch password) -> tunnel?
;; Launches an instance of the Seashell Tunnel backend,
;;  modified to use the UW CSCF passwordless SSH mechanism.
;;
;; Configuration options (see seashell-config.rkt):
;;  host - host to connect to.
;;
;; Expects:
;;  seashell-tunnel should live in the same directory as this file.
;;
;; Arguments:
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
(define/contract (tunnel-launch)
  (-> tunnel?)

  ;; Randomly select a host
  (define host (first (shuffle (read-config 'host))))
#|
  (define short-hostname
    (string-trim
      (with-output-to-string
        (thunk
          (system "/bin/hostname")))))
  (define host (string-append short-hostname ".hosts.seashell.student.cs.uwaterloo.ca"))
|#

  ;; Launch the process
  (define-values (process in out error)
    (subprocess #f #f #f
#|
                (read-config 'seashell-backend-remote)))
|#
                (read-config 'ssh-binary)
                "-x"
                "-o" "PreferredAuthentications hostbased"
                "-o" (format "GlobalKnownHostsFile ~a" (read-config 'seashell-known-hosts))
                host
                (read-config 'seashell-backend-remote)))

  ;; And the logger thread
  (define status-thread
    (thread
     (lambda ()
       (let loop ()
         (define line (read-line error))
         (when (not (eq? line eof))
           (logf 'debug "tunnel stderr (~a:~a): ~a" (getenv "USER") host line)
           (loop))
         ;; EOF received - die.
         (close-input-port error)))))

  ;; Set unbuffered mode for the ports, so nothing funny happens.
  (file-stream-buffer-mode out 'none)

  ;; All good.
  (tunnel process in out status-thread host))
