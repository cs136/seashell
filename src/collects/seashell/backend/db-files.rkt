#lang typed/racket

(require typed/json)
(require typed/db)
(require typed/db/sqlite3)

(require/typed file/zip
  [zip (-> (U String Path) (U String Path) Void)])

(require seashell/backend/template)
(require seashell/db/seashell)

(provide new-project
         delete-project
         new-file
         new-directory
         export-project)

;; just setting something up so I can test for now
;(unless (file-exists? "test.db")
;  (disconnect (sqlite3-connect #:database "test.db" #:mode 'create)))

;; What I'm thinking:
;;
;; Tables:
;; contents: id, project_id, file_id, contents, time
;; files: id, project_id, name, contents_id, flags
;; projects: id, name, settings, last_used

;; TODO: put the database rows into structures so we don't need to do as much type casting

;; TODO support adding project from template
;; Returns new project ID
(: new-project (-> String String))
(define (new-project name)
  (insert-new "projects"
    #{`#hasheq((name . ,name)
               (settings . ,#{(hash) :: JSExpr})
               (last_used . ,(current-milliseconds))) :: (HashTable Symbol JSExpr)}))

(: delete-project (-> String Void))
(define (delete-project id)
  (call-with-write-transaction (thunk
    (delete-id "projects" id)
    (delete-files-for-project id))))

;; TODO: new-file should fail if the file name already exists
(: new-file (-> String String (U String False) Integer (Values String (U String False))))
(define (new-file pid name contents flags)
  (define result (call-with-write-transaction (thunk
    (when (filename-exists? pid name)
      (error (format "A file with the name '~a' already exists." name)))
    (define contents-id (if contents (get-uuid) #f))
    (define file-id (insert-new "files"
      `#hasheq((project_id . ,pid)
               (name . ,name)
               (contents_id . ,contents-id)
               (flags . ,flags))))
    (when contents (insert-new "contents"
      `#hasheq((project_id . ,pid)
               (file_id . ,file-id)
               (contents . ,contents)
               (time . ,(current-milliseconds))) contents-id))
    (cons file-id contents-id))))
  (values (car result) (cdr result)))

(: new-directory (-> String String String))
(define (new-directory pid name)
  (define-values (fid cid) (new-file pid name #f 0))
  fid)

(: export-file (-> JSExpr (U String Path) Void))
(define (export-file file proj-dir)
  (define contents (select-id "contents" (cast (hash-ref (cast file (HashTable Symbol JSExpr)) 'contents_id) String)))
  (when contents
    (with-output-to-file (build-path proj-dir (cast (hash-ref (cast file (HashTable Symbol JSExpr)) 'name) String))
      (thunk (printf "~a" (cast (hash-ref (cast contents (HashTable Symbol JSExpr)) 'contents) String))))))

(: export-directory (-> JSExpr (U String Path) Void))
(define (export-directory dir proj-dir)
  (define path (build-path proj-dir (cast (hash-ref (cast dir (HashTable Symbol JSExpr)) 'name) String)))
  (unless (directory-exists? path) (make-directory path)))

(: zip-from-dir (-> String (U String Path) Void))
(define (zip-from-dir target dir)
  (define cur (current-directory))
  (current-directory dir)
  (zip (if (relative-path? target) (build-path cur target) target)
       ".")
  (current-directory cur))

(: export-project (-> String Boolean String Void))
(define (export-project pid zip? target)
  (call-with-read-transaction (thunk
    (define files (select-files-for-project pid))
    (define tmpdir (make-temporary-file "rkttmp~a" 'directory))
    (map (lambda ([d : JSExpr]) (export-directory d tmpdir))
         (sort (filter (lambda ([x : JSExpr]) (not (cast (hash-ref (cast x (HashTable Symbol JSExpr)) 'contents_id) (U String False))))
                       files)
               (lambda ([a : JSExpr] [b : JSExpr]) (string<? (cast (hash-ref (cast a (HashTable Symbol JSExpr)) 'name) String)
                                                             (cast (hash-ref (cast b (HashTable Symbol JSExpr)) 'name) String)))))
    (map (lambda ([f : JSExpr]) (export-file f tmpdir))
         (filter (lambda ([x : JSExpr]) (cast (hash-ref (cast x (HashTable Symbol JSExpr)) 'contents_id) (U String False)))
                 files))
    (printf "~s\n" files)
    (cond
      [zip?
        (when (file-exists? target) (delete-file target))
        (zip-from-dir target tmpdir)]
      [else
        (when (directory-exists? target) (delete-directory/files target))
        (copy-directory/files tmpdir target)])
    (delete-directory/files tmpdir))))
