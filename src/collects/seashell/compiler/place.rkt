#lang racket
;; Seashell's Clang interface.
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
(require seashell/compiler/compiler)

(provide seashell-compile-files/place)

;; seashell-compile-files/place 
;; Invokes seashell-compile-files in a separate place,
;; preserving parallelism in Racket.
;;
;; Caution: There is some nasty behaviour with places and the FFI.
;; Until this is resolved, do not use this function.
(define/contract (seashell-compile-files/place user-cflags user-ldflags sources objects)
  (-> (listof string?) (listof string?) (listof path?) (listof path?)
      (values (or/c bytes? false?) (hash/c path? (listof seashell-diagnostic?))))
  (define compiler-place (dynamic-place 'seashell/compiler/place-main
                                        'seashell-compiler-place))
  (place-channel-put compiler-place user-cflags)
  (place-channel-put compiler-place user-ldflags)
  (place-channel-put compiler-place sources)
  (place-channel-put compiler-place objects)
  (define result (place-channel-get compiler-place))
  (define data (place-channel-get compiler-place))
  (values result data))
