;; Seashell
;; Copyright (C) 2012-2013 Jennifer Wong, Marc Burns
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
;;
;; Authors: Jennifer Wong, Marc Burns
(module seashell-common racket
  (require web-server/servlet
           json)
  (provide embed-jquery
           request-path-string
           response/json
           make-api-error)

  ;; Embed jQuery in a page.
  (define embed-jquery
    `(script [(type "text/javascript")
              (src  "https://ajax.googleapis.com/ajax/libs/jquery/1.8.2/jquery.min.js")]))

  ;; Convert list of pairs to a JSON response.
  (define (response/json assocs)
    (response/full 200 #"Good" (current-seconds)
                   TEXT/HTML-MIME-TYPE
                   empty
                   (list (jsexpr->bytes (make-hash assocs)))))

  ;; Make a JSON response indicating an API error.
  (define (make-api-error code str)
    (response/json
      `((error . ,(make-hash `((code . ,code) (reason . ,str)))))))

  ;; Convert request to path string.
  (define (request-path-string req)
    (foldl
     (lambda(c b)
       (string-append b "/" (path/param-path c))) ""
                                                  (url-path (request-uri req)))))
