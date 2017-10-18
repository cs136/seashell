#lang typed/racket
;; Seashell's backend server.
;; Copyright (C) 2013-2015 The Seashell Maintainers.
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
(provide init-projects
         new-project
         delete-project
         compile-and-run-project
         compile-and-run-project/db
         export-project
         export-project-name
         export-all
         marmoset-submit
         marmoset-test-results
         archive-projects
         zip-from-dir
         (struct-out exn:project))

(require seashell/log
         (submod seashell/seashell-config typed)
         seashell/compiler
         seashell/backend/runner
         seashell/backend/files
         seashell/db/tools
         seashell/utils/misc
         net/url
         net/head
         typed/json
         file/zip
         file/unzip
         seashell/backend/exception)

(struct exn:project exn:seashell:backend ())

(require/typed file/zip
  [zip (-> (U String Path) (U String Path) Void)])
(require/typed file/unzip
  [unzip (-> (U String Input-Port) (-> Bytes Boolean Input-Port Any) Void)]
  [call-with-unzip (All (A) (-> Input-Port (-> Path A) A))])
(require/typed racket/hash
  [hash-union (-> (HashTable Symbol JSExpr) (HashTable Symbol JSExpr) (HashTable Symbol JSExpr))])

(require/typed seashell/backend/template
  [call-with-template (All (A) (-> String (-> Input-Port A) A))])

;; (runtime-files-path)
;; Returns the path where runtime files are stored.
(: runtime-files-path (-> String))
(define (runtime-files-path)
  (path->string (build-path (read-config-string 'runtime-files-path))))

;; (init-projects)
;; Creates directories necessary to run Seashell
(: init-projects (-> Void))
(define (init-projects)
  (make-directory* (runtime-files-path))
  (void))

;; (read-metadata file)
;; Parses the .metadata file when importing a project from a template.
;;
;; Args:
;;  file - path to the metadata file
;; Returns:
;;  Two values: a list of (list filename flags) and the project settings hash
(: read-metadata (-> Path-String (Values (Listof (List String Integer)) (HashTable Symbol JSExpr))))
(define (read-metadata file)
  (match-define (cons flags settings)
    (foldl (lambda ([pair : (Pairof Symbol JSExpr)] [res : (Pairof (Listof (List String Integer)) (Listof (Pairof Symbol JSExpr)))])
      #{(match (car pair)
        ['flags (cons (cast (cdr pair) (Listof (List String Integer))) (cdr res))]
        [setting (cons (car res) (cons (cons setting (cdr pair)) (cdr res)))])
          :: (Pairof (Listof (List String Integer)) (Listof (Pairof Symbol JSExpr)))})
      #{(cons '() '()) :: (Pairof (Listof (List String Integer)) (Listof (Pairof Symbol JSExpr)))}
      (hash->list (cast (string->jsexpr (file->string file)) (HashTable Symbol JSExpr)))))
  (values flags (make-hash settings)))

;; (new-project name template? settings?)
;; Creates a new project.
;;
;; Args:
;;  name - name of the new project
;;  template - (Optional) location of the project template zip file to clone.
;;             If not provided, uses a default project template.
;;  settings - (Optional) hash of project settings for new project.
;;             Will be merged with the hash from the metadata file (if applicable)
;; Returns:
;;  New project ID
(: new-project (->* (String) ((U String False) (HashTable Symbol JSExpr)) String))
(define (new-project name [template #f] [settings #{(hash) :: (HashTable Symbol JSExpr)}])
  (call-with-write-transaction (thunk
    (when (project-exists? name)
      (raise (exn:project (format "The project ~a already exists." name)
        (current-continuation-marks))))
    (call-with-template (if template template (read-config-string 'default-project-template))
      (lambda ([port : Input-Port])
        (call-with-unzip port
          (lambda ([dir : Path])
            (parameterize ([current-directory (build-path dir (first (directory-list dir)))])
              (define-values (flags meta-settings)
                (if (file-exists? (read-config-path 'project-settings-filename))
                    (read-metadata (read-config-path 'project-settings-filename))
                    (values '() #{(hash) :: (HashTable Symbol JSExpr)})))
              (define pid (insert-new "projects"
                #{`#hasheq((name . ,name)
                           (settings . ,(cast (hash-union settings meta-settings) JSExpr))
                           (last_used . ,(current-milliseconds))) :: (HashTable Symbol JSExpr)}))
              (for-each (lambda ([p : Path])
                (cond
                  [(directory-exists? p)
                    (new-directory pid (path->string p))]
                  [(not (string=? (read-config-string 'project-settings-filename)
                                  (path->string p)))
                    (define stored-flags (filter (lambda ([fl : (List String Integer)])
                      (string=? (first fl) (path->string p)))
                      flags))
                    (new-file pid
                              (path->string p)
                              (file->string p)
                              (if (empty? stored-flags)
                                  0
                                  (second (first stored-flags))))
                    (void)]))
                (sequence->list (in-directory)))
              pid))))))))

;; (delete-project id)
;; Deletes a project.
;;
;; Args:
;;  id - project ID to delete
(: delete-project (-> String Void))
(define (delete-project id)
  (call-with-write-transaction (thunk
    (delete-id "projects" id)
    (delete-files-for-project id))))

;; (compile-and-run-project location file question-name tests test-location)
;; Compiles and runs a project.
;;
;; Arguments:
;;  location - directory where the project is located
;;  file - Full path and name of file we are compiling from
;;         When called from compile-and-run-project/use-runner below, looks like
;;         "q1/file.rkt" or "common/file.rkt"
;;  question-name - Name of the question we are running
;;  test - Name of test, or empty to denote no test.
;;  test-location - One of:
;;    'tree - Look for the tests in <directory containing file>/test.
;;    'flat - Look for the tests in <directory containing file>.
;;    'current-directory - Look for the tests in <current-directory>
;;    path-string? - Look for the tests in <project-dir>/<test-loc>.
;;    By default, 'tree.
;; Returns:
;;  A boolean, denoting if compilation passed/failed.
;;  A hash-map, with the following bindings:
;;    status - one of "running", "compile-failed"
;;    message - Compilation error messages/warnings.
;;    pid - Resulting PID
;; Raises:
;;  exn:project if project does not exist.
(: compile-and-run-project (->* (String String String (Listof String)) ((U 'tree 'flat 'current-directory String)) (Values Boolean (HashTable Symbol JSExpr))))
(define (compile-and-run-project location file question-name tests [test-location 'tree])
  (when (not (directory-exists? location))
    (raise (exn:project (format "Project ~a does not exist!" location)
                        (current-continuation-marks))))

  (define project-base location)
  (define project-base-str (path->string (path->complete-path project-base)))
  (define project-common
    (build-path project-base (read-config-string 'common-subdirectory)))
  (define project-question (build-path project-base question-name))

  (define project-common-list
    (if (directory-exists? project-common)
      (directory-list project-common #:build? #t)
      '()))
  ;; Figure out the real test location.
  (define real-test-location
    (cond
      [(path-string? test-location)
       (build-path project-base test-location)]
      [else test-location]))
  ;; Figure out which language to run with
  (define lang
    (match (filename-extension file)
      ;; TODO: allow students to run .o files as well?
      ['#"rkt" 'racket]
      ['#"c" 'C]
      [_ (raise (exn:project "You can only run .c or .rkt files!"
            (current-continuation-marks)))]))
  ;; Base path, and basename of the file being run
  (match-define-values (base exe _)
    (split-path (build-path project-base file)))
  ;; Check if we're running a file in common folder
  (define running-common-file?
    (let ([dlst (cast (explode-path file) (Listof Path))])
      (string=? "common" (path->string (first dlst)))))

  (define (compile-c-files)
    ;; Run the compiler - save the binary to (runtime-files-path) $name-$file-binary
    ;; if everything succeeds.
    (define-values (result messages)
      (seashell-compile-files/place (read-config-strings 'compiler-flags)
                                    '("-lm")
                                    `(,project-question
                                      ,@(if (directory-exists? project-common) (list project-common) empty))
                                    (build-path project-base file)))
    (define output-path (build-path (runtime-files-path) (format "~a-~a-binary" (file-name-from-path file) (gensym))))
    (when result
      (with-output-to-file output-path
                           #:exists 'replace
                           (lambda ()
                             (write-bytes result)))
      (file-or-directory-permissions
        output-path
        (bitwise-ior (file-or-directory-permissions output-path 'bits) user-execute-bit)))

    ;; Parse the messages.
    (define parsed-messages
      (apply append
        (hash-map
          messages
          (lambda ([key : Path] [diagnostics : (Listof Seashell-Diagnostic)])
                  (map
                    (lambda ([diagnostic : Seashell-Diagnostic])
                      (list (seashell-diagnostic-error? diagnostic)
                            (some-system-path->string (find-relative-path (simple-form-path project-base)
                                                                          (if (path-string? (seashell-diagnostic-file diagnostic))
                                                                              (simple-form-path (seashell-diagnostic-file diagnostic))
                                                                              (simple-form-path key))))
                            (seashell-diagnostic-line diagnostic)
                            (seashell-diagnostic-column diagnostic)
                            (seashell-diagnostic-message diagnostic)))
                    diagnostics)))))
      (values result parsed-messages output-path))

  (define (flatten-racket-files)
    ;; Create a temporary directory
    (define temp-dir (make-temporary-file "seashell-racket-runner-temp-~a" 'directory))
    ;; Copy the common folder to the temp dir -- for backward compatibility this term
    (when (directory-exists? project-common)
      (copy-directory/files project-common (build-path temp-dir (read-config-string 'common-subdirectory))))

    (cond [running-common-file?
           ;; Copy the files over from the question into the common folder
           (merge-directory/files (build-path project-base question-name) (build-path temp-dir (read-config-string 'common-subdirectory)))
           ;; In case students want to do (require "../qX/file.txt") from their common file
           (merge-directory/files (build-path temp-dir (read-config-string 'common-subdirectory))
                                  (build-path temp-dir question-name))]
          [else
           ;; Copy the files over from the question
           (merge-directory/files (cast base Path) (build-path temp-dir question-name))
           ;; Copy all files in the common folder to the question folder
           (when (directory-exists? project-common)
             (merge-directory/files project-common (build-path temp-dir question-name)))])
    (values (build-path temp-dir) (build-path temp-dir question-name)))

  (define-values (racket-temp-dir
                  racket-target-dir)
    (if (equal? lang 'racket)
      (flatten-racket-files)
      (values (build-path ".") (build-path "."))))

  (define-values (result messages target)
    (match lang
      ['C (compile-c-files)]
      ['racket (values #t '() (build-path racket-target-dir exe))]))

  (cond
    [(and result (empty? tests))
      (define pid (run-program target (cast base Path) project-base-str lang #f real-test-location))
      (thread
        (lambda ()
          (sync (program-wait-evt pid))
          (match lang
            ['C (delete-directory/files target #:must-exist? #f)]
            ['racket (delete-directory/files racket-temp-dir #:must-exist? #f)])))
      (values #t `#hash((pid . ,pid) (messages . ,messages) (status . "running")))]
    [result
      (define pids (map
                     (lambda ([test : String])
                       (run-program target (cast base Path) project-base-str lang test real-test-location))
                     tests))
      (thread
        (lambda ()
          (let loop ([evts (map program-wait-evt pids)])
            (unless (empty? evts)
              (loop (remove (apply sync evts) evts))))
          (match lang
            ['C (delete-directory/files target #:must-exist? #f)]
            ['racket (delete-directory/files racket-temp-dir #:must-exist? #f)])))
      (values #t `#hash((pids . ,pids) (messages . ,messages) (status . "running")))]
    [else
      (values #f `#hash((messages . ,messages) (status . "compile-failed")))]))

;; (compile-and-run-project/db pid question tests)
;; Exports the given project, then compiles and runs it with compile-and-run-project.
;; Runner file is determined by the project settings in the database.
;;
;; Args:
;;  pid - project id we are running
;;  question - question name we are running
;;  tests - list of the names of tests to run (not suffixed with .in/.expect)
;; Returns:
;;  Same as compile-and-run-project
(: compile-and-run-project/db (-> String String (Listof String) (Values Boolean (HashTable Symbol JSExpr))))
(define (compile-and-run-project/db pid question tests)
  (define tmpdir (make-temporary-file "seashell-compile-tmp-~a" 'directory))
  (match-define (cons res hsh) (dynamic-wind
    void
    (thunk (define run-file (call-with-read-transaction (thunk
        (define proj (select-id "projects" pid))
        (unless proj (raise (exn:fail (format "Project with id ~a does not exist." pid)
                                      (current-continuation-marks))))
        (define name (cast (hash-ref (cast proj (HashTable Symbol JSExpr)) 'name) String))
        (export-project pid #f tmpdir)
        (hash-ref (cast (hash-ref (cast proj (HashTable Symbol JSExpr)) 'settings) (HashTable Symbol String))
              (string->symbol (string-append question "_runner_file"))))))
      (define path (build-path tmpdir run-file))
      (define-values (res hsh)
        (compile-and-run-project (path->string tmpdir) run-file question tests))
      (when res
        (define pids (cast (hash-ref hsh 'pids
                                     (thunk (list (cast (hash-ref hsh 'pid) Integer))))
                           (Listof Integer)))
        ;; Wait until the program finishes to kill test.
        (thread
          (lambda ()
            (let loop ([evts (map program-wait-evt pids)])
              (unless (empty? evts)
                (loop (remove (apply sync evts) evts))))
            (delete-directory/files tmpdir))))
      (cons res hsh))
    void))
  (values res hsh))

;; (zip-from-dir target dir)
;; Creates a .zip from the directory provided
;;
;; Args:
;;  target - path of .zip file to create
;;  dir - directory we are zipping
(: zip-from-dir (-> Path-String Path-String Void))
(define (zip-from-dir target dir)
  (define cur (current-directory))
  (current-directory dir)
  (zip (if (relative-path? target) (build-path cur target) target)
       ".")
  (current-directory cur))

;; (export-project pid zip? target)
;; Exports a project from the database to the filesystem
;;
;; Args:
;;  pid - project ID to export
;;  zip? - boolean, #t to export as a .zip, #f to export as a directory
;;  target - path to .zip or directory to export to
(: export-project (->* (String Boolean Path-String) ((U String False)) Void))
(define (export-project pid zip? target [question #f])
  (call-with-read-transaction (thunk
    (define files (select-files-for-project pid))
    (define tmpdir (make-temporary-file "seashell-export-~a" 'directory))
    ;; export-file will ensure that the necessary directories are created
    ;; create all the regular files
    ;; compile a list of flags as we go
    (define flags (foldl (lambda ([f : JSExpr] [flags : (Listof (List String Integer))])
        (cond [(hash? f)
               (define name (cast (hash-ref (cast f (HashTable Symbol JSExpr)) 'name) String))
               (cond [(false? question)
                      ; question is false => export everything
                      (export-file f tmpdir)
                      (define flg (cast (hash-ref (cast f (HashTable Symbol JSExpr)) 'flags) Integer))
                      (if (zero? flg)
                          flags
                          (cons (list name flg) flags))]
                     [(and (string? question)
                           (or (string-prefix? name (read-config-string 'common-subdirectory))
                               (string-prefix? name (read-config-string 'tests-subdirectory))
                               (string-prefix? name (string-append question "/"))))
                      ; question is string => export only the question's files, common/ and tests/
                      (if (string-prefix? name (string-append question "/"))
                          (export-file (cast ((inst hash-set Symbol JSExpr) f 'name (string-trim name (string-append question "/") #:right? #f)) JSExpr)
                                       tmpdir)
                          (export-file f tmpdir))
                      (define flg (cast (hash-ref (cast f (HashTable Symbol JSExpr)) 'flags) Integer))
                      (if (zero? flg)
                          flags
                          (cons (list name flg) flags))]
                     [else flags])]
              [else flags])) ; end foldl's lambda function
      '()
      (filter (lambda ([x : JSExpr]) (cast (hash-ref (cast x (HashTable Symbol JSExpr)) 'contents_id) (U String False)))
              files)))
    (define project (select-id "projects" pid))
    (unless project
      (raise (exn:project "The project being exported does not exist." (current-continuation-marks))))
    (define psettings (cast (hash-ref (cast project (HashTable Symbol JSExpr)) 'settings) (HashTable Symbol JSExpr)))
    (with-output-to-file (build-path tmpdir (read-config-path 'project-settings-filename))
      (thunk (display (jsexpr->string (hash-set psettings 'flags flags)))))
    (cond
      [zip?
        (when (file-exists? target) (delete-file target))
        (zip-from-dir target tmpdir)
        (delete-directory/files tmpdir)]
      [else
        (when (directory-exists? target) (delete-directory/files target))
        (copy-directory/files tmpdir target)
        (delete-directory/files tmpdir)]))))

;; (export-project-name name zip? target)
;; Exports the project with the given name from the database to the filesystem.
;; Looks up the name in the projects table then calls export-project.
;;
;; Args:
;;  name - name of project to export
;;  zip?, target - same as export-project
(: export-project-name (-> String Boolean String Void))
(define (export-project-name name zip? target)
  (call-with-read-transaction (thunk
    (define proj (select-project-name name))
    (export-project (cast (hash-ref (cast proj (HashTable Symbol JSExpr)) 'id) String) zip? target))))

;; (export-all target)
;; Exports all projects in the database to the filesystem.
;;
;; Args:
;;  target - path of directory to export everything under
(: export-all (-> Path-String Void))
(define (export-all target)
  (unless (directory-exists? target)
    (make-directory target))
  (define projects (select-projects))
  (map (lambda ([p : JSExpr])
    (export-project (cast (hash-ref (cast p (HashTable Symbol JSExpr)) 'id) String)
      #f (path->string (build-path target (cast (hash-ref (cast p (HashTable Symbol JSExpr)) 'name) String)))))
    projects)
  (void))

;; (marmoset-submit course assn pid question)
;; Submits a question to Marmoset.
;;
;; Args:
;;  course - name of the course, used in SQL query (i.e. "CS136")
;;  assn - name of the assignment/project in marmoset
;;  pid - project ID
;;  question - name of question to submit
(: marmoset-submit (-> String String String (U False String) Void))
(define (marmoset-submit course assn pid question)
  (logf 'info (format "Called (marmoset-submit ~a ~a ~a ~a)" course assn pid question))
  (: tmpzip (U False Path))
  (define tmpzip #f)
  (dynamic-wind
    (lambda ()
      (set! tmpzip (make-temporary-file "seashell-marmoset-zip-~a")))
    (lambda ()
      (export-project pid #t (assert tmpzip) question)

      ;; Launch the submit process.
      (define-values (proc out in err)
        (subprocess #f #f #f (read-config-string 'submit-tool) course assn (assert tmpzip)))

      ;; Wait until it's done.
      (subprocess-wait proc)
      (define stderr-output (port->string err))
      (define stdout-output (port->string out))
      (define exit-status (cast (subprocess-status proc) Integer))
      (close-output-port in)
      (close-input-port out)
      (close-input-port err)

      ;; Report errors
      (unless (zero? exit-status)
        (raise (exn:project (format "Could not submit project - marmoset_submit returned ~a: (~a) (~a)"
                                    exit-status
                                    stderr-output stdout-output)
                            (current-continuation-marks)))))
    (lambda ()
      (delete-directory/files (assert tmpzip) #:must-exist? #f))))

;; (marmoset-test-results course project type)
;; Retrieves the results for the given project from Marmoset.
;;
;; Args:
;;  course - name of course in Marmoset
;;  project - name of Marmoset project
;;  type - 'public or 'secret tests
;; Returns:
;;  JSON string containing the Marmoset results
(: marmoset-test-results (-> String String (U 'public 'secret) String))
(define (marmoset-test-results course project type)
  ;; Run the script that gets test results
  (define-values (proc out in err)
    (subprocess #f #f #f (read-config-string 'test-results-tool) (symbol->string type) project))
  (subprocess-wait proc)
  (define stderr-output (port->string err))
  (define stdout-output (port->string out))
  (define exit-status (cast (subprocess-status proc) Number))
  (close-output-port in)
  (close-input-port out)
  (close-input-port err)

  (if (not (zero? exit-status))
      (jsexpr->string (hash 'error #t 'result empty))
      stdout-output))

;; (archive-projects archive-name)
;; Moves all existing project files into a directory called archive-name
;;
;; Args:
;;  archive-name - name of new db file to archive to, or #f to use timestamp
(: archive-projects (-> (U False String) Void))
(define (archive-projects archive-name)
  (define arch-file (build-path (read-config-string 'seashell) "archives"
    (if archive-name archive-name (string-append (number->string (current-seconds)) ".db"))))
  (define arch-root (build-path (read-config-string 'seashell) "archives"))
  (define db-file (build-path (read-config-string 'seashell) (read-config-string 'database-file)))
  (unless (directory-exists? arch-root)
    (make-directory arch-root))
  (call-with-write-transaction (thunk
    ;; copy the current database
    (backup-database (path->string arch-file))
    (logf 'info (format "Archived database to ~a" arch-file))
    ;; delete everything from the database
    (delete-everything))))
