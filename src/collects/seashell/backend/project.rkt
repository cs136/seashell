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
         (struct-out exn:project)
         check-path
         init-projects
         check-and-build-path
         build-project-path
         project-base-path
         runtime-files-path
         compile-and-run-project
         compile-and-run-project/use-runner
         marmoset-submit
         marmoset-test-results
         get-most-recently-used
         update-most-recently-used
         export-project
         archive-projects
         get-file-to-run
         set-file-to-run
         read-project-settings 
         write-project-settings
         write-project-settings/key
         add-open-file
         remove-open-file
         get-open-files)

(require seashell/log
         seashell/seashell-config
         seashell/compiler
         seashell/backend/runner
         seashell/backend/template
         seashell/backend/lock
         seashell/utils/misc
         net/url
         net/head
         json
         file/zip
         file/unzip
         racket/contract
         racket/function
         racket/file
         racket/path
         racket/match
         racket/string
         racket/list
         racket/port
         racket/set
         racket/system)

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
  (build-path (read-config 'runtime-files-path)))

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
;;  * A URL to a ZIP file.
;;  * A path to a ZIP file.
;;
;; Arguments:
;;  name - Name of the new project.
;;  source - See above.
;;
;; Raises:
;;  exn:project if the project already exists.
(define/contract (new-project-from name source)
  (-> project-name? (or/c project-name? url-string? path-string?) void?)
  (with-handlers
    ([exn:fail:filesystem?
       (lambda (exn)
         (raise (exn:project
                  (format "Project already exists, or some other filesystem error occurred: ~a" (exn-message exn))
                  (current-continuation-marks))))])
    (call-with-write-lock (thunk
      (cond
        [(or (path-string? source) (url-string? source))
          (make-directory (build-project-path name))
          (with-handlers
            ([exn:fail? (lambda (exn)
              (delete-directory/files (build-project-path name) #:must-exist? #f)
              (raise exn))])
            (parameterize ([current-directory (build-project-path name)])
              (call-with-template source
                (lambda (port) (unzip port (make-filesystem-entry-reader #:strip-count 1))))))]
        [(project-name? source)
         (copy-directory/files (build-project-path source)
                               (build-project-path name))]))))
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
    (call-with-write-lock (thunk
      (delete-directory/files (check-and-build-path (build-project-path name))))))
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

;; (compile-and-run-project name file question-name tests full-path test-location)
;; Compiles and runs a project.
;;
;; Arguments:
;;  name - Name of project.
;;  file - Full path and name of file we are compiling from
;;         When called from compile-and-run-project/use-runner below, looks like
;;         "q1/file.rkt" or "common/file.rkt"
;;  question-name - Name of the question we are running
;;  test - Name of test, or empty to denote no test.
;;  full-path - If #f, looks for the project in the standard project location.
;;                 #t, assumes name is the full path to the project directory.
;;    By default, #f.
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
(define/contract (compile-and-run-project name file question-name tests [full-path #f] [test-location 'tree])
  (->* (path-string? (or/c #f path-string?) string? (listof path-string?))
       (boolean? (or/c path-string? 'tree 'flat 'current-directory))
       (values boolean? hash?))
  (when (or (and (not full-path) (not (is-project? name)))
            (and full-path (not (directory-exists? name))))
    (raise (exn:project (format "Project ~a does not exist!" name)
                        (current-continuation-marks))))

  (define project-base (if full-path name (build-project-path name)))
  (define project-base-str (path->string (path->complete-path project-base)))
  (define project-common (if full-path
    (build-path project-base (read-config 'common-subdirectory))
    (check-and-build-path project-base (read-config 'common-subdirectory))))
  (define project-question (build-path project-base question-name))

  (define project-common-list
    (if (directory-exists? project-common)
      (directory-list project-common #:build? #t)
      '()))
  ;; Figure out the real test location.
  (define real-test-location
    (cond
      [(path-string? test-location)
       (check-and-build-path project-base test-location)]
      [else test-location]))
  ;; Figure out which language to run with
  (define lang
    (match (filename-extension file)
      ;; TODO: allow students to run .o files as well?
      ['#"rkt" 'racket]
      ['#"c" 'C]
      [_ (error "You can only run .c or .rkt files!")]))
  ;; Base path, and basename of the file being run
  (match-define-values (base exe _)
    (split-path (check-and-build-path project-base file)))
  ;; Check if we're running a file in common folder
  (define running-common-file?
    (let ([dlst (explode-path file)])
      (string=? "common" (path->string (first dlst)))))

  (define (compile-c-files)
    ;; Run the compiler - save the binary to (runtime-files-path) $name-$file-binary
    ;; if everything succeeds.
    (define-values (result messages)
      (seashell-compile-files/place (read-config 'compiler-flags)
                                    '("-lm")
                                    `(,project-question
                                      ,@(if (directory-exists? project-common) (list project-common) empty))
                                    (check-and-build-path project-base file)))
    (define output-path (check-and-build-path (runtime-files-path) (format "~a-~a-binary" (file-name-from-path file) (gensym))))
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
    (define temp-dir (make-temporary-file "seashell-racket-temp-~a" 'directory))
    ;; Copy the common folder to the temp dir -- for backward compatibility this term
    (when (directory-exists? project-common)
      (copy-directory/files project-common (build-path temp-dir (read-config 'common-subdirectory))))

    (cond [running-common-file?
           ;; Copy the files over from the question into the common folder
           (merge-directory/files (build-path project-base question-name) (build-path temp-dir (read-config 'common-subdirectory)))
           ;; In case students want to do (require "../qX/file.txt") from their common file
           (merge-directory/files (build-path temp-dir (read-config 'common-subdirectory))
                                  (build-path temp-dir question-name))]
          [else
           ;; Copy the files over from the question
           (merge-directory/files base (build-path temp-dir question-name))
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
      ['racket (values #t '() (check-and-build-path racket-target-dir exe))]))

  (cond
    [(and result (empty? tests))
      (define pid (run-program target base project-base-str lang #f real-test-location))
      (thread
        (lambda ()
          (sync (program-wait-evt pid))
          (match lang
            ['C (delete-directory/files target #:must-exist? #f)]
            ['racket (delete-directory/files racket-temp-dir #:must-exist? #f)])))
      (values #t `#hash((pid . ,pid) (messages . ,messages) (status . "running")))]
    [result
      (eprintf "about to test\n")
      (define pids (map
                     (lambda (test)
                       (run-program target base project-base-str lang test real-test-location))
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


;; (compile-and-run-project/use-runner name tests)
;; is a wrapper around compile-and-run-project, supplying the file
;; from the project settings file.
;;
;; Arguments:
;;  name: Name of project (eg. "A10")
;;  question: Name of the question (eg. "q1")
;;  tests: Tests for the project.
(define/contract (compile-and-run-project/use-runner name question tests)
  (-> project-name? string? (listof path-string?)
      (values boolean?
              hash?))
  (define file-to-run (get-file-to-run name question))
  (if (string=? file-to-run "")
    (raise (exn:project (format "Question \"~a\" does not have a runner file." question)
                        (current-continuation-marks)))
    (compile-and-run-project name file-to-run question tests #f (build-path question (read-config 'tests-subdirectory)))))


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
            (define (copy-from! base include-hidden [dest #f])
              (fold-files
                (lambda (path type _)
                  (define file (last (explode-path path)))
                  (cond
                    [(equal? path base) (values #t #t)]
                    ;; do not submit hidden files to Marmoset
                    [(and (not include-hidden) (string-prefix? (path->string file) "."))
                      (values #t #t)]
                    [else
                      (match
                        type
                        ['dir
                         (make-directory
                           (find-relative-path base path))
                         (values #t #t)]
                        ['file
                         (copy-file path
                                    (if dest (build-path dest file)
                                             (find-relative-path base path)))
                         (values #t #t)]
                        [_ (values #t #t)])]))
                #t
                base))
            
            (copy-from! question-dir #f)
            (when (directory-exists? common-dir)
              (define compath (check-and-build-path tmpdir "common"))
              (make-directory compath)
              (copy-from! common-dir #f compath))
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

;; (marmoset-test-results project)
(define/contract (marmoset-test-results course project type)
  (-> string? string? (or/c 'public 'secret) string?)

  ;; Run the script that gets test results
  (define-values (proc out in err)
    (subprocess #f #f #f (read-config 'test-results-tool) (symbol->string type) project))
  (subprocess-wait proc)
  (define stderr-output (port->string err))
  (define stdout-output (port->string out))
  (define exit-status (subprocess-status proc))
  (close-output-port in)
  (close-input-port out)
  (close-input-port err)

  (if (not (zero? exit-status))
      (jsexpr->string (hash 'error #t 'result empty))
      stdout-output))

;; (get-most-recently-used project question)
;; Reads the most recently used information for the specified project/question.
;;
;; Arguments:
;;  project - the project to look in
;;  question - the directory to check the information in, #f if at root.
;; Returns:
;;  Either the most recently used information, or #f if not set yet.
(define/contract (get-most-recently-used project question)
  (-> (and/c project-name? is-project?) (or/c #f path-string?) (or/c #f string?))
  (define key (if question (string->symbol (string-append question "_most_recently_used")) 'most_recently_used))
  (define file (read-project-settings/key project key))
  (define path (if file (check-and-build-path (build-project-path project) file) #f))
  (if (and file (or (and question (file-exists? path))
                    (and (not question) (directory-exists? path)))) file #f))

;; (update-most-recently-used project question file)
;; Updates the most recently used information for the specified question.
;;
;; Arguments:
;;  project - the project to update.
;;  question - the directory to update, or #f if at root.
;;  file - The data to write.
;; Returns:
;;  Nothing.
(define/contract (update-most-recently-used project question file)
  (-> (and/c project-name? is-project?) (or/c #f path-string?) path-string? void?)
  (define key (if question (string->symbol (string-append question "_most_recently_used")) 'most_recently_used))
  (write-project-settings/key project key file))

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

;; Lists the questions in a given project
(define/contract (list-questions project)
  (-> (and/c project-name? is-project?) (listof (and/c string? path-string?)))
  (define start-path (build-project-path project))
  (filter (lambda (q) (and (directory-exists? q) (not (equal? (build-path start-path "common") q))))
    (directory-list (build-project-path project))))

;; (read-project-settings project)
;; Reads the project settings from the project root.
;;
;; For now, this fetches all of the most recently used files separately.
;; As a result of this, most recently used data is stored redundantly
;; in the project settings file.
;; TODO: consolidate most recently used purely as a project setting
;; 
;; Returns:
;;   settings - the project settings
(define/contract (read-project-settings project)
  (-> (and/c project-name? is-project?) hash-eq?)
  (define filename (read-config 'project-settings-filename))
  (cond 
    [(file-exists? (build-path (build-project-path project) filename))
     (define res (with-input-from-file 
       (build-path (build-project-path project) filename) read))
     (if (eof-object? res) (hasheq) res)]
    [else (hasheq)]))

;; (read-project-settings/key project key)
;; Retrieves the value of a specific key in the project settings.
;; Returns false if the key does not exist.
(define/contract (read-project-settings/key project key)
  (-> (and/c project-name? is-project?) symbol? (or/c #f string? number?))
  (hash-ref (read-project-settings project) key #f))


;; (write-project-settings project settings)
;; Writes to the project settings in the project root.
;;
;; Arguments: settings, a hash of all the project settings
;;
;; Returns: nothing
(define/contract (write-project-settings project settings)
  (-> (and/c project-name? is-project?) hash-eq? void?)
  (call-with-write-lock (thunk
    (with-output-to-file
      (build-path (build-project-path project) (read-config 'project-settings-filename))
      (lambda () (write settings))
      #:exists 'replace))))


;; (write-project-settings/key project key val)
;; Updates the (key, val) pair in the project settings hash.
;; Equivalent to a hash-set.
;; Returns: nothing
(define/contract (write-project-settings/key project key val)
  (-> (and/c project-name? is-project?) symbol? (or/c string? number?) void?)
  (define old-settings (read-project-settings project)) 
  (define new-settings 
    (hash-set (if old-settings old-settings #hasheq())  key val))
  (write-project-settings project new-settings))


;; (get-file-to-run project question) attempts to read the 
;;   runner settings file, that specifies which file to run.
;;
;; Params:
;;   project - name of the project (eg. "A10")
;;   question - basename of the question (eg. "q2")
;;
;; Returns:
;;   A string indicating the file to run
(define/contract (get-file-to-run project question)
  (-> (and/c project-name? is-project?) path-string? (or/c path-string? ""))
  (define file-to-run
    (read-project-settings/key project
              (string->symbol (string-append question "_runner_file"))))
  (cond 
    [file-to-run
      (if (not (file-exists? (build-path (build-project-path project)
                                          file-to-run)))
        (raise (exn:project (format "File ~a does not exist." file-to-run)
                            (current-continuation-marks)))
        file-to-run)]
    [else 
      (raise (exn:project 
               (format "Question \"~a\" does not have a runner file." question)
               (current-continuation-marks)))]))

  
;; (set-file-to-run project question folder file) writes to the question
;;   settings file, specifying which file to run.
;; 
;; Params:
;;   project - the path of the project
;;   question - the basename of the question (eg. "q2")
;;   folder - the basename of the folder of the file (eg. "q2")
;;            used to prevent people from setting a test as a runner
;;   file - the basename of the file to run (eg. "main.c")
;;
;; Returns:
;;   Nothing
(define/contract (set-file-to-run project question folder file)
  (-> (and/c project-name? is-project?) path-string? path-string? path-string? void)
  (if (string=? folder (read-config 'tests-subdirectory))
    (raise (exn:project (format "You cannot set a runner file in the ~a folder." folder) 
                        (current-continuation-marks)))
    (write-project-settings/key project
                                (string->symbol (string-append question "_runner_file"))
                                (path->string (build-path (if (string=? folder (read-config 'common-subdirectory))
                                                              (read-config 'common-subdirectory)
                                                              question)
                                                          file)))))

(define/contract (add-open-file project question path)
  (-> (and/c project-name? is-project?) path-string? path-string? void)
  (write-project-settings/key project (string->symbol (string-append question "_open_files"))
                              (jsexpr->string `(,path ,@(get-open-files project question)))))

(define/contract (get-open-files project question)
  (-> (and/c project-name? is-project?) path-string? (or/c #f (listof path-string?)))
  (filter
    (lambda (file)
      (file-exists? (build-path (build-project-path project) file)))
    (remove-duplicates (string->jsexpr (or
      (read-project-settings/key project (string->symbol (string-append question "_open_files"))) "[]")))))

(define/contract (remove-open-file project question path)
  (-> (and/c project-name? is-project?) path-string? path-string? void)
  (write-project-settings/key project (string->symbol (string-append question "_open_files"))
                              (jsexpr->string (remove* `(,path) (get-open-files project question)))))
