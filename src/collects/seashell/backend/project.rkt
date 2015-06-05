#lang racket/base
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
(struct exn:project exn:fail:user ())

(provide project-name?
         url-string?
         list-projects
         is-project?
         new-project
         new-project-from
         delete-project
         lock-project
         force-lock-project
         unlock-project
         exn:project?
         exn:project
         check-path
         init-projects
         check-and-build-path
         build-project-path
         project-base-path
         runtime-files-path
         compile-and-run-project
         marmoset-submit
         get-most-recently-used
         update-most-recently-used
         export-project
         archive-projects)

(require seashell/log
         seashell/seashell-config
         seashell/compiler
         seashell/backend/runner
         net/url
         net/head
         json
         file/zip
         file/unzip
         racket/contract
         racket/file
         racket/path
         racket/match
         racket/string
         racket/list
         racket/port)

;; Global variable, which is a set of currently locked projects
(define locked-projects (make-hash))
(define lock-semaphore (make-semaphore 1))

;; (project-name? name) -> bool?
;; Predicate for testing if a string is a valid project name.
(define (project-name? name)
  (cond
    [(not (or (path-string? name) (string? name))) #f]
    [(let-values ([(base suffix _) (split-path name)])
      (and (equal? base 'relative) (path-for-some-system? suffix)))
     #t]
    [else #f]))

;; (is-project? name)
;; Checks if name is a project that exists.
;;
;; Arguments:
;;  name - Name of project.
;; Returns: #t if it does, #f otherwise.
(define/contract (is-project? name)
  (-> project-name? boolean?)
  (directory-exists? (build-project-path name)))

;; (check-path path)
;; Makes sure nothing funny is in a path.  Currently deals with .. ('up)
;; This function should be called on any path that depends on user input.
;;
;; Arguments:
;;  path - Path to check.
;;
;; Returns:
;;  path if OK.
;;
;; Raises:
;;  exn:project if a bad path is given.
;;
;; Notes:
;;  Use the special form (check-and-build-path ...) to check the result of build-path.
(define/contract (check-path path)
  (-> path? path?)
  (define/contract (check-path-recursive current)
    (-> path? void?)
    (define-values (base name _) (split-path current))
    (when (equal? name 'up)
          (raise (exn:project (format "Invalid path ~a!" path)
                              (current-continuation-marks))))
    (when (path? base) (check-path-recursive base)))
  (check-path-recursive path)
  path)
(define-syntax-rule (check-and-build-path args ...)
  (check-path (build-path args ...)))


;; (url-string? str) -> bool?
;; Predicate for testing if a string is a valid URL
(define/contract (url-string? str)
  (-> string? boolean?)
  (with-handlers
    ([url-exception? (lambda (exn) #f)])
    (string->url str)
    #t))

;; (project-base-path)
;; Gets the base path where projects are located
(define/contract (project-base-path)
  (-> path?)
  (build-path (read-config 'seashell) "projects"))

;; (build-project-path project)
;; Gets the path where project is stored
(define/contract (build-project-path project)
  (-> project-name? path?)
  (check-and-build-path (project-base-path) project))

;; (runtime-files-path)
;; Gets the path where runtime files are stored.
(define/contract (runtime-files-path)
  (-> path?)
  (build-path (read-config 'seashell) "runtime-files"))

;; (init-projects)
;; Creates the directories for projects
(define/contract (init-projects)
  (-> void?)
  (make-directory* (runtime-files-path))
  (make-directory* (project-base-path))
  (void))

;; list-projects -> (listof (listof project-name? number?))
;; Lists existing Seashell projects.
(define/contract (list-projects)
  (-> (listof (list/c project-name? any/c)))
  (map (lambda (proj) (list (some-system-path->string proj) (file-or-directory-modify-seconds (build-project-path proj))))
       (filter (compose directory-exists? build-project-path)
               (directory-list (project-base-path)))))

;; (new-project name) -> void?
;; Creates a new project.
;;
;; Arguments:
;;  name - Name of the new project.
;;
;; Raises:
;;  exn:project if the project already exists.
(define/contract (new-project name)
  (-> project-name? void?)
  (with-handlers
    ([exn:fail:filesystem?
       (lambda (exn)
         (raise (exn:project
                  (format "Project already exists, or some other filesystem error occurred: ~a" (exn-message exn))
                  (current-continuation-marks))))])
    (new-project-from name (read-config 'default-project-template)))
  (void))

;; (new-project-from name source)
;; Creates a new project from a source.
;;
;; source is a string which can be the following:
;;  * A old project, in which we clone it directly.
;;  * A URI, in which we clone the URI.  This is useful for setting up
;;    the base files for a given CS 136 assignment question.
;;
;; Arguments:
;;  name - Name of the new project.
;;  source - See above.
;;
;; Raises:
;;  exn:project if the project already exists.
(define/contract (new-project-from name source)
  (-> project-name? (or/c project-name? url-string?) void?)
  (with-handlers
    ([exn:fail:filesystem?
       (lambda (exn)
         (raise (exn:project
                  (format "Project already exists, or some other filesystem error occurred: ~a" (exn-message exn))
                  (current-continuation-marks))))])
    (cond
      [(url-string? source)
        (make-directory (build-project-path name))
        (with-handlers
          ([exn:fail?
             (lambda (exn)
               (delete-directory/files (build-project-path name) #:must-exist? #f)
               (raise exn))])
          (parameterize ([current-directory (build-project-path name)])
            (define surl (string->url source))
            (cond
              [(equal? (url-scheme surl) "file")
               (call/input-url surl get-pure-port
                               (lambda (port)
                                 (unzip port (make-filesystem-entry-reader #:strip-count 1))))]
              [else
               (define-values (port hdrs) (get-pure-port/headers surl #:status? #t #:redirections 10))
               (dynamic-wind
                 (lambda () #f)
                 (lambda ()
                   (match-define (list _ status text headers) (regexp-match #rx"^HTTP/1\\.1 ([0-9][0-9][0-9]) ([^\n\r]*)(.*)" hdrs))
                   (when (not (equal? status "200"))
                     (raise (exn:project (format "Error when fetching template ~a for project ~a: ~a ~a." source name status text)
                                         (current-continuation-marks))))
                   (when (not (equal? (string-trim (extract-field "Content-Type" headers)) "application/zip"))
                     (raise (exn:project (format "Error when fetching template ~a for project ~a: template was not a ZIP file." source name)
                                         (current-continuation-marks))))
                   (unzip port (make-filesystem-entry-reader #:strip-count 1)))
                 (lambda () (close-input-port port)))])))]
      [(project-name? source)
       (copy-directory/files (build-project-path source)
                             (build-project-path name))]))
  (void))

;; (delete-project name)
;; Deletes a project.
;;
;; Arguments:
;;  name - Name of the project.
;;
;; Raises:
;;  exn:project if the project does not exist.
(define/contract (delete-project name)
  (-> project-name? void?)
  (with-handlers
    ([exn:fail:filesystem?
       (lambda (exn)
         (raise (exn:project
                  (format "Project does not exists, or some other filesystem error occurred: ~a" (exn-message exn))
                  (current-continuation-marks))))])
    (delete-directory/files (check-and-build-path (build-project-path name))))
  (void))

;; (lock-project name)
;; Locks a project.
;;
;; Arguments:
;;  name - Name of the project.
;;  thread-to-lock-on - Thread to lock on.
;;
;; Raises:
;;  exn:project if the project does not exist.
;;
;; Returns:
;;  #t if the project was successfully locked, #f otherwise.
;; Notes:
;;  Project is automatically unlocked if the thread dies.
(define/contract (lock-project name thread-to-lock-on)
  (-> (and/c project-name? is-project?) thread? boolean?)
  (call-with-semaphore
    lock-semaphore
    (lambda ()
      (when (thread-dead? (hash-ref! locked-projects name thread-to-lock-on))
        (hash-remove! locked-projects name))
      (define unlocked (eq? (hash-ref! locked-projects name thread-to-lock-on) thread-to-lock-on))
      (when unlocked
        (file-or-directory-modify-seconds (build-project-path name)
                                          (current-seconds)))
      unlocked)))

;; (force-lock-project name)
;; Forcibly locks a project, even if it is already locked
;;
;; Arguments:
;;  name - Name of the project.
;;  thread-to-lock-on - Thread to lock on.
;;
;; Raises:
;;  exn:project if the project does not exist.
;; Notes:
;;  Project is automatically unlocked if the thread dies.
(define/contract (force-lock-project name thread-to-lock-on)
  (-> (and/c project-name? is-project?) thread? void?)
  (call-with-semaphore
    lock-semaphore
    (lambda ()
      (file-or-directory-modify-seconds (build-project-path name)
                                        (current-seconds))
      (hash-set! locked-projects name thread-to-lock-on))))

;; (unlock-project name)
;; Unlocks a project.
;;
;; Arguments:
;;  name - Name of the project.
;;
;; Raises:
;;  exn:project if the project does not exist, or an error occurred.
(define/contract (unlock-project name)
  (-> (and/c project-name? is-project?) boolean?)
  (call-with-semaphore
    lock-semaphore
    (lambda ()
      (cond
        [(hash-has-key? locked-projects name)
          (hash-remove! locked-projects name) #t]
        [else (raise (exn:project (format "Could not unlock ~a!" name) (current-continuation-marks)))]))))

;; (compile-and-run project name file tests)
;; Compiles and runs a project.
;;
;; Arguments:
;;  name - Name of project.
;;  file - Full path and name of file we are compiling from, or #f
;;         to denote no file.  (We attempt to do something reasonable in this case).
;;  test - Name of test, or empty to denote no test.
;;
;; Returns:
;;  A boolean, denoting if compilation passed/failed.
;;  A hash-map, with the following bindings:
;;    status - one of "running", "compile-failed"
;;    message - Compilation error messages/warnings.
;;    pid - Resulting PID
;; Raises:
;;  exn:project if project does not exist.
(define/contract (compile-and-run-project name file tests)
  (-> project-name? (or/c #f path-string?) (listof path-string?)
      (values boolean?
              hash?))
  (when (not (is-project? name))
    (raise (exn:project (format "Project ~a does not exist!" name)
                        (current-continuation-marks))))

  (define project-base (build-project-path name))
  (define project-common (check-and-build-path project-base (read-config 'common-subdirectory)))
  (define project-common-list
    (if (directory-exists? project-common)
      (directory-list project-common #:build? #t)
      '()))

  ;; Figure out which language to run with
  (define lang
    (match (filename-extension file)
      ['#"rkt" 'racket]
      ['#"c" 'C]
      ['#"h" 'C] ;; TODO: this is a temporary fix, which we figure out racket vs. C running
      [_ (error "You can only run .c or .rkt files!")]))
  ;; Base path.
  (match-define-values (base _ _)
                       (if file (split-path (check-and-build-path project-base file))
                                (values (check-and-build-path project-base) #f #f)))

  (define (compile-c-files)
    ;; TODO: Other languages? (C++, maybe?)
    ;; Get *.c files in project.
    (define c-files
      (filter (lambda (file)
                (equal? (filename-extension file) #"c"))
              (append
                (directory-list base #:build? #t)
                project-common-list)))
    (define o-files
      (filter (lambda (file)
                (equal? (filename-extension file) #"o"))
              (append
                (directory-list base #:build? #t)
                project-common-list)))
    ;; Run the compiler - save the binary to (runtime-files-path) $name-$file-binary
    ;; if everything succeeds.
    (define-values (result messages)
      (seashell-compile-files/place `(,@(read-config 'compiler-flags)
                                      ,@(if (directory-exists? project-common) `("-I" ,(some-system-path->string project-common)) '()))
                                    '("-lm") c-files o-files))
    (define output-path (check-and-build-path (runtime-files-path) (format "~a-~a-~a-binary" name (file-name-from-path file) (gensym))))
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
          (lambda (key diagnostics)
                  (map
                    (lambda (diagnostic)
                      (list (seashell-diagnostic-error? diagnostic)
                            (some-system-path->string (find-relative-path (simple-form-path project-base)
                                                              (simple-form-path key)))
                            (seashell-diagnostic-line diagnostic)
                            (seashell-diagnostic-column diagnostic)
                            (seashell-diagnostic-message diagnostic)))
                    diagnostics)))))
      (values result parsed-messages output-path))

  (define-values (result messages target)
    (match lang
      ['C (compile-c-files)]
      ['racket (values #t '() (check-and-build-path project-base file))]))

  (cond
    [(and result (empty? tests))
      (define pid (run-program target base lang #f))
      (when (equal? lang 'C)
        (thread
          (lambda ()
            (sync (program-wait-evt pid))
            (delete-directory/files target #:must-exist? #f))))
      (values #t `#hash((pid . ,pid) (messages . ,messages) (status . "running")))]
    [result
      (define pids (map
                     (lambda (test)
                       (run-program target base lang test))
                     tests))
      (when (equal? lang 'C)
        (thread
          (lambda ()
            (let loop ([evts (map program-wait-evt pids)])
              (unless (empty? evts)
                (loop (remove (apply sync evts) evts))))
            (delete-directory/files target #:must-exist? #f))))
      (values #t `#hash((pids . ,pids) (messages . ,messages) (status . "running")))]
    [else
      (values #f `#hash((messages . ,messages) (status . "compile-failed")))]))

;; (export-project name) -> bytes?
;; Exports a project to a ZIP file.
;;
;; Arguments:
;;  name - Name of project.
;; Returns:
;;  zip - ZIP file as a bytestring.
(define/contract (export-project name)
  (-> project-name?
      bytes?)
  (when (not (is-project? name))
    (raise (exn:project (format "Project ~a does not exist!" name)
                        (current-continuation-marks))))
  (parameterize
    ([current-directory (project-base-path)])
    (define output-port (open-output-bytes))
    (parameterize
      ([current-output-port output-port])
      (zip->output (pathlist-closure (list name))))
    (get-output-bytes output-port)))

;; (marmoset-submit course assn project file) -> void
;; Submits a file to marmoset
;;
;; Arguments:
;;   course  - Name of the course, used in SQL query (i.e. "CS136")
;;   assn    - Name of the assignment/project in marmoset
;;   project - Name of the project (of the file to be submitted) in seashell
;;   subdirectory - Name of subdirectory/question to submit, or #f
;;                  to submit everything.
(define/contract (marmoset-submit course assn project subdirectory)
  (-> string? string? (and/c project-name? is-project?) (or/c #f path-string?) void?)

  (define tmpzip #f)
  (define tmpdir #f)

  (dynamic-wind
    (lambda ()
      (set! tmpzip (make-temporary-file "seashell-marmoset-zip-~a"))
      (set! tmpdir (make-temporary-file "seashell-marmoset-build-~a"
                                         'directory)))
    (lambda ()
      (cond
        ;; Two cases - either we're submitting a subdirectory...
        [subdirectory
          ;; Here's what we do to ensure correct linkage.
          ;;
          ;; common/filesA*                 filesA
          ;; question/filesB*          -->  filesB
          ;; question/tests/tests*          tests/

          ;; TODO what to do with duplicate file names in common/ and in question/?
          ;; Right now we toss an exception.
          (define project-dir
            (build-project-path project))
          (define question-dir
            (check-and-build-path project-dir subdirectory))
          (define common-dir
            (build-path project-dir (read-config 'common-subdirectory)))
          (parameterize ([current-directory tmpdir])
            (define (copy-from! base)
              (fold-files
                (lambda (path type _)
                  (cond
                    [(equal? path base) (values #t #t)]
                    [else
                      (match
                        type
                        ['dir
                         (make-directory
                           (find-relative-path base path))
                         (values #t #t)]
                        ['file
                         (copy-file path
                                    (find-relative-path base path))
                         (values #t #t)]
                        [_ (values #t #t)])]))
                #t
                base))
            
            (copy-from! question-dir)
            (when (directory-exists? common-dir)
              (copy-from! common-dir))
            (with-output-to-file
              tmpzip
              (lambda () (zip->output (pathlist-closure (directory-list))))
              #:exists 'truncate))]
        ;; Or we're submitting the entire project.
        [else
          (with-output-to-file
            tmpzip
            (lambda () (write-bytes (export-project project)))
            #:exists 'truncate)])

      ;; Launch the submit process.
      (define-values (proc out in err)
        (subprocess #f #f #f (read-config 'submit-tool) course assn tmpzip))

      ;; Wait until it's done.
      (subprocess-wait proc)
      (define stderr-output (port->string err))
      (define stdout-output (port->string out))
      (define exit-status (subprocess-status proc))
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
      (delete-directory/files tmpzip #:must-exist? #f)
      (delete-directory/files tmpdir #:must-exist? #f))))

;; (get-most-recently-used project directory)
;; Reads the most recently used information for the specified project/question.
;;
;; Arguments:
;;  project - the project to look in
;;  directory - the directory to check the information in, #f if at root.
;; Returns:
;;  Either the most recently used information, or #f if not set yet.
(define/contract (get-most-recently-used project directory)
  (-> (and/c project-name? is-project?) (or/c #f path-string?) jsexpr?)
  (define recent (build-path (read-config 'seashell) "recent.txt"))
  (define directory-path (if (not directory)
                             (build-project-path project)
                             (check-and-build-path (build-project-path project) directory)))
  (define directory-hash (some-system-path->string (if (not directory) (check-and-build-path project) (check-and-build-path project directory))))
  (cond
   [(not (file-exists? recent)) #f]
   [(not (directory-exists? directory-path)) #f]
   [else
     (let/ec escape
       (match-define `(,predicate ,data) (hash-ref (with-input-from-file recent read)
                                                   directory-hash
                                                   (lambda () (escape #f))))
       (match predicate
         [`("dexists" ,name)
           (if (directory-exists? (check-and-build-path (build-project-path project) name)) data #f)]
         [`("fexists" ,name)
           (if (file-exists? (check-and-build-path (build-project-path project) name)) data #f)]))]))

;; (update-recent project directory data)
;; Updates the most recently used information for the specified directory.
;;
;; Arguments:
;;  project - the project to update.
;;  directory - the directory to update, or #f if at root.
;;  predicate - A predicate, either:
;;            ("dexists" name)
;;            ("fexists" name)
;;  data - The data to write.
;; Returns:
;;  Nothing.
(define/contract (update-most-recently-used project directory predicate data)
  (-> (and/c project-name? is-project?) (or/c #f path-string?) (list/c (or/c "dexists" "fexists") path-string?) jsexpr? void?)
  (define recent-file (build-path (read-config 'seashell) "recent.txt"))
  (define recent-hash 
    (if (file-exists? recent-file) (with-input-from-file recent-file read) `#hash()))
  (define directory-path (if (not directory)
                             (build-project-path project)
                             (check-and-build-path (build-project-path project) directory)))
  (define directory-hash (some-system-path->string (if (not directory) (check-and-build-path project) (check-and-build-path project directory))))
  (when (directory-exists? directory-path)
    (with-output-to-file recent-file 
                         (lambda ()
                           (write
                             (hash-set (if (hash? recent-hash) recent-hash `#hash())
                                       directory-hash
                                       (list predicate data))))
                         #:exists 'truncate))
  (void))

;; (archive-projects archive-name) moves all existing project files into a
;;   directory called archive-name
;;
;; Params:
;;   archive-name - name of new folder to archive to, or #f to use timestamp
;;
;; Returns:
;;   Nothing
(define/contract (archive-projects archive-name)
  (-> (or/c #f path-string?) void?)
  (define dir-path (check-and-build-path (read-config 'seashell) "archives"
    (if archive-name archive-name (number->string (current-seconds)))))
  (define arch-root (build-path (read-config 'seashell) "archives"))
  (define proj-root (build-path (read-config 'seashell) "projects"))
  (unless (directory-exists? arch-root)
    (make-directory arch-root))
  (rename-file-or-directory proj-root dir-path)
  (make-directory proj-root))
