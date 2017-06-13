#lang typed/racket

(require typed/json
         seashell/db/tools
         seashell/utils/uuid)

(require (submod seashell/seashell-config typed))

(require/typed file/zip
  [zip (-> (U String Path) (U String Path) Void)])
(require/typed file/unzip
  [call-with-unzip (All (A) (-> Input-Port (-> Path A) A))])

(require/typed seashell/backend/template
  [call-with-template (All (A) (-> String (-> Input-Port A) A))])
(require/typed seashell/backend/project
  [compile-and-run-project (->* (Path-String Path-String String (Listof String)) (Boolean) (Values Boolean (HashTable Symbol JSExpr)))])

(provide new-project
         delete-project
         new-file
         new-directory
         export-project
         export-project-name
         export-all
         compile-and-run-project/db)

;; What I'm thinking:
;;
;; Tables:
;; contents: id, project_id, file_id, contents, time
;; files: id, project_id, name, contents_id, flags
;; projects: id, name, settings, last_used

;; TODO: put the database rows into structures so we don't need to do as much casting

;; Returns new project ID
(: new-project (->* (String) ((U String False) (HashTable Symbol String)) String))
(define (new-project name [template #f] [settings #{(hash) :: (HashTable Symbol String)}])
  (call-with-write-transaction (thunk
    (define pid (insert-new "projects"
      #{`#hasheq((name . ,name)
                 (settings . ,(cast settings JSExpr))
                 (last_used . ,(current-milliseconds))) :: (HashTable Symbol JSExpr)}))
    (call-with-template (if template template (read-config-string 'default-project-template))
      (lambda ([port : Input-Port])
        (call-with-unzip port
          (lambda ([dir : Path])
            (parameterize ([current-directory (build-path dir (first (directory-list dir)))])
              (map (lambda ([p : Path])
                  (if (directory-exists? p)
                    (new-directory pid (path->string p))
                    (new-file pid (path->string p) (file->string p) 0)) (void))
                (sequence->list (in-directory))))))))
    pid)))

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
    (define contents-id (if contents (bytes->string/utf-8 (uuid-generate)) #f))
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

(: zip-from-dir (-> Path-String Path-String Void))
(define (zip-from-dir target dir)
  (define cur (current-directory))
  (current-directory dir)
  (zip (if (relative-path? target) (build-path cur target) target)
       ".")
  (current-directory cur))

(: export-project (-> String Boolean Path-String Void))
(define (export-project pid zip? target)
  (call-with-read-transaction (thunk
    (define files (select-files-for-project pid))
    (define tmpdir (make-temporary-file "rkttmp~a" 'directory))
    ;; filter out only directories, sort them lexicographically, then create them all.
    (map (lambda ([d : JSExpr]) (export-directory d tmpdir))
         (sort (filter (lambda ([x : JSExpr]) (not (cast (hash-ref (cast x (HashTable Symbol JSExpr)) 'contents_id) (U String False))))
                       files)
               (lambda ([a : JSExpr] [b : JSExpr]) (string<? (cast (hash-ref (cast a (HashTable Symbol JSExpr)) 'name) String)
                                                             (cast (hash-ref (cast b (HashTable Symbol JSExpr)) 'name) String)))))
    ;; then create all the regular files
    (map (lambda ([f : JSExpr]) (export-file f tmpdir))
         (filter (lambda ([x : JSExpr]) (cast (hash-ref (cast x (HashTable Symbol JSExpr)) 'contents_id) (U String False)))
                 files))
    (printf "~s\n" files)
    (cond
      [zip?
        (when (file-exists? target) (delete-file target))
        (zip-from-dir target tmpdir)
        (delete-directory/files tmpdir)]
      [else
        (when (directory-exists? target) (delete-directory/files target))
        (rename-file-or-directory tmpdir target)]))))

(: export-project-name (-> String Boolean String Void))
(define (export-project-name name zip? target)
  (call-with-read-transaction (thunk
    (define proj (select-project-name name))
    (export-project (cast (hash-ref (cast proj (HashTable Symbol JSExpr)) 'id) String) zip? target))))

(: export-all (-> String Void))
(define (export-all target)
  (unless (directory-exists? target)
    (make-directory target))
  (define projects (select-projects))
  (map (lambda ([p : JSExpr])
    (export-project (cast (hash-ref (cast p (HashTable Symbol JSExpr)) 'id) String)
      #f (path->string (build-path target (cast (hash-ref (cast p (HashTable Symbol JSExpr)) 'name) String)))))
    projects)
  (void))

;; (compile-and-run-project/db pid question tests)
;;
;; pid: project id we are running
;; question: question name we are running
;; tests: list of the names of tests to run (not suffixed with .in/.expect)
;;
;; Runner file is determined by the project settings. This function access the database
;;  then calls compile-and-run-project with the appropriate parameters.
(: compile-and-run-project/db (-> String String (Listof String) (Values Boolean (HashTable Symbol JSExpr))))
(define (compile-and-run-project/db pid question tests)
  (define tmpdir (make-temporary-file "rkttmp~a" 'directory))
  (match-define (cons res hsh) (dynamic-wind
    void
    (thunk (define run-file (call-with-read-transaction (thunk
        (define proj (select-id "projects" pid))
        (define name (cast (hash-ref (cast proj (HashTable Symbol JSExpr)) 'name) String))
        (export-project pid #f tmpdir)
        (hash-ref (cast (hash-ref (cast proj (HashTable Symbol JSExpr)) 'settings) (HashTable Symbol String))
              (string->symbol (string-append question "_runner_file"))))))
      (printf "run file: ~a\n" run-file)
      (define path (build-path tmpdir run-file))
      (printf "path: ~a\nexists: ~a\n" path (file-exists? path))
      (define-values (res hsh) (compile-and-run-project tmpdir run-file question tests #t))
      (cons res hsh))
    (thunk (delete-directory/files tmpdir))))
  (values res hsh))
