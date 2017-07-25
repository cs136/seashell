#lang typed/racket

(require typed/json
         seashell/db/tools
         seashell/utils/uuid)

(require/typed file/unzip
  [unzip (-> (U String Input-Port) (-> Bytes Boolean Input-Port Any) Void)])

(require/typed seashell/backend/template
  [call-with-template (All (A) (-> String (-> Input-Port A) A))])

(provide new-file
         new-directory
         restore-file-from-template
         export-file
         export-directory)

(: new-file (-> String String (U String False) Integer (Values String (U String False))))
(define (new-file pid name contents flags)
  (define result (call-with-write-transaction (thunk
    (when (filename-exists? pid name)
      (error (format "A file with the name '~a' already exists." name)))
    (define contents-id (if contents (uuid-generate) #f))
    (define file-id (insert-new "files"
      `#hasheq((project_id . ,pid)
               (name . ,name)
               (contents_id . ,contents-id)
               (flags . ,flags))))
    (when contents (insert-new "contents"
      `#hasheq((project_id . ,pid)
               (filename . ,name)
               (contents . ,contents)
               (time . ,(current-milliseconds))) contents-id))
    (cons file-id contents-id))))
  (values (car result) (cdr result)))

(: new-directory (-> String String String))
(define (new-directory pid name)
  (define-values (fid cid) (new-file pid name #f 0))
  fid)

;; (restore-file-from-template project file template)
;; Restores a file from a skeleton/template.
;;
;; Args:
;;  pid - project ID
;;  file - Path to file.
;;  template - Path (possibly URL) to template.
(: restore-file-from-template (-> String String String Void))
(define (restore-file-from-template pid file template)
  (: ok Boolean)
  (define ok #f)
  (define source (explode-path file))
  (call-with-template template
    (lambda ([port : Input-Port])
      (unzip port
        (lambda ([name : Bytes] [dir : Boolean] [contents : Input-Port])
          (define lname (explode-path (simplify-path (bytes->path name) #f)))
          (when (and (not (null? lname))
                     (equal? source (cdr lname)))
            (new-file pid
                      file
                      (port->string contents)
                      0)
            (set! ok #t))))))
  (unless ok
    (raise (exn:fail (format "File ~a not found in template ~a." file template)
      (current-continuation-marks)))))

(: export-file (-> JSExpr (U String Path) Void))
(define (export-file file proj-dir)
  (printf "file: ~a\n" file)
  (define contents (select-id "contents" (cast (hash-ref (cast file (HashTable Symbol JSExpr)) 'contents_id) String)))
  (when contents
    (with-output-to-file (build-path proj-dir (cast (hash-ref (cast file (HashTable Symbol JSExpr)) 'name) String))
      (thunk (printf "~a" (cast (hash-ref (cast contents (HashTable Symbol JSExpr)) 'contents) String))))))

(: export-directory (-> JSExpr (U String Path) Void))
(define (export-directory dir proj-dir)
  (define path (build-path proj-dir (cast (hash-ref (cast dir (HashTable Symbol JSExpr)) 'name) String)))
  (unless (directory-exists? path) (make-directory path)))
