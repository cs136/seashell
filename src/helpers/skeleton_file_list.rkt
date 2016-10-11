#!/u/cs136/seashell-support-files/racket/bin/racket
#lang racket
(require net/cgi
         json
         file/unzip)

(define skel-file "/u/cs136/public_html/assignment_skeletons/skeletons.json")
(define skel-template "/u/cs136/public_html/assignment_skeletons/~a-seashell.zip")
(define wl-u-file "/u/cs136/public_html/assignment_skeletons/user_whitelist.json")
(define wl-skel-file "/u/cs136/public_html/assignment_skeletons/project_whitelist.json")

(define (report-exn-and-quit exn)
  (write-json
    `#hash((error . #t)
           (result . ,(exn-message exn)))))

(define (main)
  (define bindings (get-bindings))
  (define template (extract-binding/single "template" bindings))
  (define user-list (extract-bindings "user" bindings))
  (define user (if (empty? user-list) '() (first user-list)))

  ;; Make sure template shows up in the skeletons file.
  (define wlusers (call-with-input-file wl-u-file read-json))
  (define templates (append (call-with-input-file skel-file read-json)
    (if (member user wlusers) (call-with-input-file wl-skel-file read-json) '())))
  (unless (member template templates)
    (error "Template not present in skeletons file!"))

  (write-json 
    `#hash((error . #f)
           (result . ,(map bytes->string/utf-8
                        (zip-directory-entries
                          (read-zip-directory
                            (format skel-template template))))))))

;; Entry point here.
(printf "Content-Type: text/json\r\n")
(printf "Access-Control-Allow-Origin: *\r\n\r\n")
(with-handlers
  ([exn? report-exn-and-quit])
  (main))
