#!/u/cs136/seashell-support-files/racket/bin/racket
#lang racket
(require net/cgi
         json)

(define skel-file "/u/cs136/public_html/assignment_skeletons/skeletons.json")
(define skel-template "/u/cs136/public_html/assignment_skeletons/~a-seashell.zip")

(define (report-exn-and-quit exn)
  (write-json
    `#hash((error . #t)
           (result . ,(exn-message exn)))))

(define (main)
  (define bindings (get-bindings))
  (define template (extract-binding/single "template" bindings))

  ;; Make sure template shows up in the skeletons file.
  (define templates (call-with-input-file skel-file read-json))
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
