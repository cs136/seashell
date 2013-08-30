#lang racket
;; Seashell's backend server.
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
(require json)

;; main-loop -> (nothing)
;; Main loop of the backend server.
;;
;; This function will not return.  This function
;; will exit with return code 0 if everything succeeds.
;;
;; JSON messages are read from standard input, and JSON
;; responses will be written to standard output.
;;
;; Incoming messages should be a JSON array.  The first
;; field in this array should be the message type.
;; The rest of the fields in this array are message-specific.
;;
;; Outgoing messages will be a JSON array.  The
;; the first field of this array
;; will hold the integral result code of executing the message.
;; There may be message-specific fields.
(define (main-loop)
  ;; This is probably going to have to be tweaked to deal
  ;; with I/O from running executables.  We're probably
  ;; going to have to multiplex this, and put this entire thing
  ;; in a loop with (sync)
  (define message (read-json))
  (match message
    ;; Informational messages.
    [`("version")
      (write-json
        `(200 "Seashell/0"))]
    [`("ping")
      (write-json
        `(200))]
    ;; Session control.
    [`("quit")
      (write-json
        `(200 "Exiting now!"))
      (exit 0)]
    ;; Project manipulation.
    [`("list-projects")
      (write-json
        `(200 ()))]
    [`("new-project" ,name)
      (write-json
        `(501))]
    [`("new-project-from" ,name ,old)
      (write-json
        `(501))]
    [`("delete-project" ,name)
      (write-json
        `(501))]
    [`("save-project" ,name)
      (write-json
        `(501))]
    ;; Files in projects.
    [`("new-file" ,project ,name)
      (write-json
        `(501))]
    [`("write-to-file" ,project ,name ,contents)
      (write-json
        `(501))]
    [`("read-from-file" ,project ,name)
      (write-json
        `(200 ""))]
    [`("delete-file" ,project ,name)
      (write-json
        `(501))]
    ;; Running projects
    [`("run-project" ,project)
      ;; TODO: I/O handling
      (write-json
        `(501))]
    [`("test-project" ,project)
      (write-json
        `(501))]
    [`("stop-project" ,project)
      (write-json
        `(501))])
  (main-loop))
