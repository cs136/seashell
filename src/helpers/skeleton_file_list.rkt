#!/usr/bin/racket
#lang racket
(require net/cgi
         json
         file/unzip)

(define skel-file "/u/cs145/public_html/assignment_skeletons/skeletons.json")
(define skel-template "/u/cs145/public_html/assignment_skeletons/~a-seashell.zip")
(define wl-skel-template "/u/cs145/seashell-support-files/whitelist-skeletons/~a-seashell.zip")
(define wl-u-file "/u/cs145/public_html/assignment_skeletons/user_whitelist.json")
(define wl-skel-file "/u/cs145/public_html/assignment_skeletons/project_whitelist.json")

(define (report-exn-and-quit exn)
  (write-json
    `#hash((error . #t)
           (result . ,(exn-message exn)))))

(define (main)
  (define bindings (get-bindings))
  (define template (extract-binding/single "template" bindings))
  (define user-list (extract-bindings "user" bindings))
  (define user (if (empty? user-list) '() (first user-list)))
  (define wl-list (extract-bindings "whitelist" bindings))
  (define whitelist (and (not (empty? wl-list)) (string=? (first wl-list) "true")))
  (define active-skel-template (if whitelist wl-skel-template skel-template))

  ;; Make sure template shows up in the skeletons file.
  (define wlusers (call-with-input-file wl-u-file read-json))
  (define templates (append (call-with-input-file skel-file read-json)
    (if (member user wlusers) (call-with-input-file wl-skel-file read-json) '())))
  (unless (member template templates)
    (error "Template not present in skeletons file!"))

  (define (is-not-hidden file)
    (define fname (last (string-split file "/")))
    (not (char=? #\. (string-ref fname 0))))

  (write-json 
    `#hash((error . #f)
           (result . ,(filter is-not-hidden
                         (map bytes->string/utf-8
                          (zip-directory-entries
                            (read-zip-directory
                              (format active-skel-template template)))))))))

;; Entry point here.
(printf "Content-Type: text/json\r\n")
(printf "Access-Control-Allow-Origin: *\r\n\r\n")
(with-handlers
  ([exn? report-exn-and-quit])
  (main))
