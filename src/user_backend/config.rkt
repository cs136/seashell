#!/usr/bin/racket
#lang racket
;; Seashell - a C Development Environment.
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
(provide read-config)

;; (read-config part) -> any/c
;; Reads the configuration data
;;
;; Arguments:
;;  part - Part of configuration data to read.
;; Returns:
;;  Configuration data.
(define/contract (read-config part)
  (-> symbol? any/c)
  (match part
         ['host "localhost"]
         ['seashell (build-path (find-system-path 'home-dir' ".seashell"))]
                                ))
