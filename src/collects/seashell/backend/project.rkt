#lang racket
;; Seashell's backend server.
;; Copyright (C) 2013-2014 The Seashell Maintainers.
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
         save-project
         exn:project?
         exn:project
         check-path
         init-projects
         check-and-build-path
         build-project-path
         project-base-path
         runtime-files-path
         run-project
         compile-project
         marmoset-submit
         export-project)

(require seashell/git
         seashell/log
         seashell/seashell-config
         seashell/compiler
         seashell/backend/runner
         seashell/websocket
         net/url
         file/zip)

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

;; list-projects -> (listof project-name?)
;; Lists existing Seashell projects.
(define/contract (list-projects)
  (-> (listof project-name?))
  (map some-system-path->string
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
;;  libgit2 FFI exceptions may also be raised.
(define/contract (new-project name)
  (-> project-name? void?)
  (with-handlers
    ([exn:fail:filesystem?
       (lambda (exn)
         (raise (exn:project
                  (format "Project already exists, or some other filesystem error occurred: ~a" (exn-message exn))
                  (current-continuation-marks))))])
    (seashell-git-clone/place (read-config 'default-project-template) (build-project-path name)))
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
;;  libgit2 FFI exceptions may also be raised.
(define/contract (new-project-from name source)
  (-> project-name? (or/c project-name? url-string?) void?)
  (with-handlers
    ([exn:fail:filesystem?
       (lambda (exn)
         (raise (exn:project
                  (format "Project already exists, or some other filesystem error occurred: ~a" (exn-message exn))
                  (current-continuation-marks))))])
    (seashell-git-clone/place source (build-project-path name)))
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
    (thunk
      (when (thread-dead? (hash-ref! locked-projects name thread-to-lock-on))
        (hash-remove! locked-projects name))
      (eq? (hash-ref! locked-projects name thread-to-lock-on) thread-to-lock-on))))

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
    (thunk
      (hash-set! locked-projects name thread-to-lock-on))))

;; (unlock-project name)
;; Unlocks a project.
;;
;; Arguments:
;;  name - Name of the project.
;;
;; Raises:
;;  exn:project if the project does not exist.
;;
;; Returns:
;;  #t if the project was successfully unlocked, #f otherwise
(define/contract (unlock-project name)
  (-> (and/c project-name? is-project?) boolean?)
  (call-with-semaphore
    lock-semaphore
    (thunk
      (cond
        [(hash-has-key? locked-projects name)
          (hash-remove! locked-projects name) #t]
        [else #f]))))

;; (save-project name)
;; Commits the current state of a project to Git.
;;
;; Arguments:
;;  name - Name of project.
;;  message - Message to tag the commit with.
;;
;; Raises:
;;  exn:project if project does not exist.
;;  libgit2 errors if git errors happen.
(define/contract (save-project name message)
  (-> project-name? string? void?)
  (when (not (is-project? name))
    (raise (exn:project (format "Project ~a does not exist!" name)
                        (current-continuation-marks))))
  (define repo (build-project-path name))
  ;; Here's what we do -
  ;;  1. Grab the status of the repository.
  ;;  2. Add 'adds' to each of the files that
  ;;     have been modified and created in the working tree.
  ;;  3. Add 'deletes' to each of the files that
  ;;     have been deleted from the working tree.
  ;;  4. Run the commit.
  (define status (seashell-git-get-status/place repo))
  (define entries (seashell-git-status-entrycount status))

  (define files-add
    (map (curry seashell-git-status-path status)
         (filter
           (lambda (index)
             (define flags (seashell-git-status-flags status index))
             (or (seashell-git-flag-new? flags) (seashell-git-flag-modified? flags)))
           (build-list entries values))))

  (define files-delete
    (map (curry seashell-git-status-path status)
         (filter
           (lambda (index)
             (define flags (seashell-git-status-flags status index))
             (seashell-git-flag-deleted? flags))
           (build-list entries values))))

  (logf 'info "Adding files to project ~a: ~a" name files-add)
  (logf 'info "Deleting files from project ~a: ~a" name files-delete)

  (define commit (seashell-git-make-commit repo))
  (for-each (curry seashell-git-commit-add-file commit)
            files-add)
  (for-each (curry seashell-git-commit-delete-file commit)
            files-delete)

  (seashell-git-commit/place commit message)
  (void))

;; (compile-project name)
;; Compiles a project.
;;
;; Arguments:
;;  name - Name of project.
;;  file - Full path and name of file we are compiling from, or #f,
;;         to denote no file.  (We attempt to do something reasonable in this case).
;; Returns:
;;  List of diagnostics (error?, file, line, column, message)
;;  Error if any diagnostics have error? set.
;;
;; Raises:
;;  exn:project if project does not exist.
(define/contract (compile-project name file)
  (-> project-name? (or/c #f path-string?) (values boolean? (listof (list/c boolean? string? natural-number/c natural-number/c string?))))
  (when (not (is-project? name))
    (raise (exn:project (format "Project ~a does not exist!" name)
                        (current-continuation-marks))))

  (define project-base (build-project-path name))
  (define project-common (check-and-build-path project-base (read-config 'common-subdirectory)))
  (define project-common-list
    (if (directory-exists? project-common)
      (directory-list project-common #:build? #t)
      '()))
  
  (match-define-values (base _ _)
                       (if file (split-path (check-and-build-path project-base file))
                                (values (check-and-build-path project-base) #f #f)))

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
  ;; Run the compiler - save the binary to .seashell/${name}-binary,
  ;; if everything succeeds.
  (define-values (result messages)
    (seashell-compile-files/place `("-Wall" "-Werror=int-conversion" "-Werror=int-to-pointer-cast" "-Werror=return-type"
                                    "-gdwarf-4" "-O0"
                                    ,@(if (directory-exists? project-common) `("-I" ,(some-system-path->string project-common)) '()))
                                  '("-lm") c-files o-files))
  ;; TODO Vary binary name by file we're building.
  (define output-path (check-and-build-path (runtime-files-path) (format "~a-binary" name)))
  (when result
    (with-output-to-file output-path
                         #:exists 'replace
                         (thunk
                           (write-bytes result)))
    (file-or-directory-permissions
      output-path
      (bitwise-ior (file-or-directory-permissions output-path 'bits) user-execute-bit)))

  ;; Messages is a list of seashell-diagnostic(s)
  (values
    (not (not result))
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
                  diagnostics))))))

;; (run-project name file test)
;; Runs a project
;;
;; Arguments:
;;  name - Name of project.
;;  file - Name of file to run.
;;  test - #f if running without tests, otherwise, name of test to run
;; Returns:
;;  pid - Process ID (used as unique identifier for process)
(define/contract (run-project name file test)
  (-> project-name? string? (or/c #f string?)
      (or/c integer? (list/c string? (or/c string? (hash/c symbol? string?)))))
  (when (not (is-project? name))
    (raise (exn:project (format "Project ~a does not exist!" name)
                        (current-continuation-marks))))

  ;; Figure out which language to run with
  (define lang
    (match (filename-extension file)
      ['#"rkt" 'racket]
      ['#"c" 'C]
      ['#"h" 'C] ;; TODO: this is a temporary fix, which we figure out racket vs. C running
      [_ (error "You can only run .c or .rkt files!")]))

  ;; Name of program to run.
  ;; TODO Vary by file [target] to run.
  (define program
    (match lang
      ['racket (check-and-build-path (build-project-path name) file)]
      ['C (check-and-build-path (runtime-files-path) (format "~a-binary" name))]))
 
  ;; Get the base directory of our file that we're running.
  (define project-base (build-project-path name))
  (match-define-values (base _ _)
                       (if file (split-path (check-and-build-path project-base file))
                                (values (check-and-build-path project-base) #f #f)))

  (run-program program base lang test))

;; (export-project name) -> bytes?
;; Exports a project to a ZIP file.
;;
;; Arguments:
;;  name - Name of project.
;;  #:export-git? - Export the .git directory too?
;; Returns:
;;  zip - ZIP file as a bytestring.
(define/contract (export-project name #:export-git? [export-git? #f])
  (->* (project-name?)
       (#:export-git? boolean?)
       bytes?)
  (when (not (is-project? name))
    (raise (exn:project (format "Project ~a does not exist!" name)
                        (current-continuation-marks))))
  (parameterize
    ([current-directory (build-project-path name)])
    (define output-port (open-output-bytes))
    (parameterize
      ([current-output-port output-port])
      (zip->output 
        (filter (lambda (path)
                  (or export-git?
                      (eq? path (find-relative-path ".git" path))))
                (pathlist-closure (directory-list)))))
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
  (-> string? string? string? (or/c #f string?) void?)

  (define tmpzip #f)
  (define tmpdir #f)

  (dynamic-wind
    (thunk
      (set! tmpzip (make-temporary-file "seashell-marmoset-zip-~a"))
      (set! tmpdir (make-temporary-file "seashell-marmoset-build-~a"
                                         'directory)))
    (thunk
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
              (thunk (zip->output (pathlist-closure (directory-list))))
              #:exists 'truncate))]
        ;; Or we're submitting the entire project.
        [else
          (with-output-to-file
            tmpzip
            (thunk (write-bytes (export-project project)))
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
    (thunk
      (delete-directory/files tmpzip #:must-exist? #f)
      (delete-directory/files tmpdir #:must-exist? #f))))
