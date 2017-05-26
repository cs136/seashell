#lang typed/racket

(require/typed racket/random
  [crypto-random-bytes (-> Integer Bytes)])
(require/typed file/zip
  [zip (-> (U String Path) (U String Path) Void)])
(require typed/json)
(require typed/db)
(require typed/db/sqlite3)
(require "../db/seashell.rkt")

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

(: get-uuid (-> String))
(define (get-uuid)
  ;; TODO change this to actual UUIDs
  (bytes->string/utf-8 (crypto-random-bytes 16) (integer->char (random 256))))

;; TODO support adding project from template
(: new-project (-> String String))
(define (new-project name)
  (define proj-id (get-uuid))
  (send db apply-create "projects" proj-id
    #{`#hasheq((name . ,name)
               (settings . ,#{(hash) :: JSExpr})
               (last_used . ,(current-milliseconds))) :: (HashTable Symbol JSExpr)})
  proj-id)

(: delete-project (-> String Void))
(define (delete-project id)
  (send db write-transaction (thunk
    (send db apply-delete "projects" id)
    (send db delete-files-for-project id))))

;; TODO: new-file should fail if the file name already exists
(: new-file (-> String String (U String False) Integer (values String (U String False))))
(define (new-file pid name contents flags)
  (define contents-id #{#f :: (U String False)})
  (define file-id (send db write-transaction (thunk
    (when (send db filename-exists? pid name)
      (error (format "A file with the name '~a' already exists." name)))
    (define file-id (get-uuid))
    ;; Unfortunately have to use mutation because Typed Racket can't seem to
    ;;  express a thunk that returns multiple values
    (set! contents-id (if contents (get-uuid) #f))
    (when contents (send db apply-create "contents" (cast contents-id String)
      #{`#hasheq((project_id . ,pid)
                 (file_id . ,file-id)
                 (contents . ,contents)
                 (time . ,(current-milliseconds))) :: (HashTable Symbol JSExpr)}))
    (send db apply-create "files" file-id
      #{`#hasheq((project_id . ,pid)
                 (name . ,name)
                 (contents_id . ,contents-id)
                 (flags . ,flags)) :: (HashTable Symbol JSExpr)})
    file-id)))
    (values file-id contents-id))

(: new-directory (-> String String String))
(define (new-directory pid name)
  (define-values (fid cid) (new-file pid name #f 0))
  fid)

(: export-file (-> JSExpr (U String Path) Void))
(define (export-file file proj-dir)
  (define contents (send db fetch "contents" (cast (hash-ref (cast file (HashTable Symbol JSExpr)) 'contents_id) String)))
  (with-output-to-file (build-path proj-dir (cast (hash-ref (cast file (HashTable Symbol JSExpr)) 'name) String))
    (thunk (printf "~a" (cast (hash-ref (cast contents (HashTable Symbol JSExpr)) 'contents) String)))))

(: export-directory (-> JSExpr (U String Path) Void))
(define (export-directory dir proj-dir)
  (make-directory (build-path proj-dir (cast (hash-ref (cast dir (HashTable Symbol JSExpr)) 'name) String))))

(: zip-from-dir (-> String (U String Path) Void))
(define (zip-from-dir target dir)
  (define cur (current-directory))
  (current-directory dir)
  (zip (if (relative-path? target) (build-path cur target) target)
       ".")
  (current-directory cur))

(: export-project (-> String Boolean String Void))
(define (export-project pid zip? target)
  (send db read-transaction (thunk
    (define files (send db fetch-files-for-project pid))
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

;; Tests
(define pid (new-project "A1"))
(define did (new-directory pid "q1"))
(define-values (fid cid) (new-file pid "q1/main.c" "int main(void) {\n  return 0;\n}\n" 0))
(export-project pid #f "export")
(export-project pid #t "proj.zip")

(define pid2 (new-project "Tut00"))
(define did2 (new-directory pid2 "example"))
(define-values (fid2 cid2) (new-file pid2 "example/main.c" "int main() {\n  return 0;\n}\n" 0))
(delete-project pid2)
(export-project pid2 #f "export2")
