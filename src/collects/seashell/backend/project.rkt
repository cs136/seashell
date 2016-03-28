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
         compile-and-run-project/use-runner
         marmoset-submit
         get-most-recently-used
         update-most-recently-used
         export-project
         archive-projects
         get-file-to-run
         set-file-to-run
         read-project-settings 
         write-project-settings
         write-project-settings/key
         )

(require seashell/log
         seashell/seashell-config
         seashell/compiler
         seashell/backend/runner
         seashell/backend/template
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
         racket/port
         racket/set)

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
    (cond
      [(or (path-string? source) (url-string? source))
        (make-directory (build-project-path name))
        (with-handlers
          ([exn:fail?
             (lambda (exn)
               (delete-directory/files (build-project-path name) #:must-exist? #f)
               (raise exn))])
          (parameterize ([current-directory (build-project-path name)])
            (call-with-template source
                                (lambda (port)
                                  (unzip port (make-filesystem-entry-reader #:strip-count 1))))))]
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

;; (get-co-files/rec main-file file-dir common-dir
;; Produces a list of the user's compilation files, recursively resolving
;; dependencies
;;
;; Arguments:
;;  c-files    - The .c files being compiled
;;  file-dir   - The directory containing main-file
;;  common-dir - The directory containing the common subdirectory
;;
;; Returns:
;;  A list of the .h files included by a program, with the .h extension stripped
(define/contract (get-co-files/rec c-files o-files file-dir common-dir depth)
  (-> (listof path-string?) (listof path-string?) path? path? exact-nonnegative-integer? 
      (values (listof path-string?) (listof path-string?)))

  (logf 'debug "c-files is ~s\n" c-files)
  (logf 'debug "o-files is ~s\n" o-files)

  (define headers (get-headers c-files file-dir common-dir))
  (logf 'debug "Header files are ~s." headers)

  (define-values (found-c-files found-o-files) (get-co-files headers))
  (logf 'debug ".c files are ~s." found-c-files)
  (logf 'debug ".o files are ~s." found-o-files)

  (cond
    ;; TODO: off by one on depth? subset? works w.r.t. path-string? vs string?
    [(or (> depth (read-config 'header-search-depth)) 
         (and (subset? found-c-files c-files)
              (subset? found-o-files o-files))) 
     (values c-files o-files)]
    [else 
      (get-co-files/rec (remove-duplicates (append c-files found-c-files)) 
                       (remove-duplicates (append o-files found-o-files))
                       file-dir common-dir (add1 depth))]))


;; (get-headers c-files file-dir common-dir)
;; Produces a list of the user's header files included by the files in c-files (without recursively
;; resovling dependencies
;;
;; Arguments:
;;  c-files    - The .c files being compiled
;;  file-dir   - The directory containing main-file
;;  common-dir - The directory containing the common subdirectory
;;
;; Returns:
;;  A list of the .h files included by the files in c-files, with the .h extension stripped
;; Raises:
;;  exn:project if an included header is not a .h file
;; TODO: need to clean up the subprocess and ports?
(define/contract (get-headers c-files file-dir common-dir)
  (-> (listof path-string?) path? path? (listof path-string?))
  (define clang-error (open-output-file "/dev/null" #:exists 'truncate))
  (define-values (clang clang-output clang-input fake-error)
    ;; TODO: is 'system-linker the right binary?
    (apply subprocess #f #f clang-error (read-config 'system-linker) `("-E" ,@c-files "-I" ,common-dir)))
  (define files
    (remove-duplicates
      (filter values
        (for/list ([line (in-lines clang-output)])
          (match (regexp-match #rx"^# [0-9]+ \"([^<][^\"]*)\"" line)
            [(list _ file)
              (match-define-values (hdrpath hdrname _) (split-path file))
              (cond
                [(and (or (equal? (path->directory-path hdrpath) (path->directory-path file-dir))
                          (equal? (path->directory-path hdrpath) (path->directory-path common-dir)))
                      (regexp-match #rx"\\.h$" hdrname))
                  (substring file 0 (- (string-length file) 2))]
                [else #f])]
            [#f #f])))))
  (close-input-port clang-output)
  (close-output-port clang-input)
  (close-output-port clang-error)
  files)

;; (get-co-files headers)
;; Produces a list of the local .c and .o files to be compiled/linked with a program
;;
;; Arguments:
;;  headers - The list of included local .h files, i.e., produced by get-headers
;;
;; Returns:
;;  A list of the .c files and a list of the .o files to be compiled/linked with a program.
;; Raises:
;;  exn:project if an element of headers is not a .h file, if a .h file has no
;;  corresponding .o or .c file, or if a .h file has both a .c and .o file.
(define/contract (get-co-files headers)
  (-> (listof path-string?) (values (listof path?) (listof path?)))

  ;; TODO: object/c files must be in the same directory as the header
  (logf 'debug "headers in get-co-files: ~s\n" headers)
  (for/fold ([c-files '()]
             [o-files '()])
            ([hdr headers])
    (match-define-values (basedir hdrname _) (split-path hdr))
    (match/values (values (file-exists? (string-append hdr ".c"))
                          (file-exists? (string-append hdr ".o")))
      [(#t #t) (raise (exn:project (format "You included ~a.h, but provided both ~a.c and ~a.o"
                                            hdrname hdrname hdrname)
                                   (current-continuation-marks)))]
      [(#t #f) (values (cons (string->path (string-append hdr ".c")) c-files) o-files)]
      [(#f #t) (values c-files (cons (string->path (string-append hdr ".o")) o-files))]
      [(#f #f) (raise (exn:project (format "You included ~a.h, but did not provide ~a.c or ~a.o"
                                          hdrname hdrname hdrname)
                                   (current-continuation-marks)))])))

;; (compile-and-run-project name file tests is-cli)
;; Compiles and runs a project.
;;
;; Arguments:
;;  name - Name of project.
;;  file - Full path and name of file we are compiling from
;;  test - Name of test, or empty to denote no test.
;;  is-cli - if #t, assumes all paths are relative to the current directory
;;
;; Returns:
;;  A boolean, denoting if compilation passed/failed.
;;  A hash-map, with the following bindings:
;;    status - one of "running", "compile-failed"
;;    message - Compilation error messages/warnings.
;;    pid - Resulting PID
;; Raises:
;;  exn:project if project does not exist.
(define/contract (compile-and-run-project name file tests is-cli)
  (-> path-string? (or/c #f path-string?) (listof path-string?) boolean?
      (values boolean? hash?))
  (when (and (not is-cli) (not (is-project? name)))
    (raise (exn:project (format "Project ~a does not exist!" name)
                        (current-continuation-marks))))

  (define project-base (if is-cli name (build-project-path name)))
  (define project-common (if is-cli
    (build-path project-base (read-config 'common-subdirectory))
    (check-and-build-path project-base (read-config 'common-subdirectory))))

  (define project-common-list
    (if (directory-exists? project-common)
      (directory-list project-common #:build? #t)
      '()))

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

  (match-define-values (_ question-dir-name _) (split-path base))

  (define (compile-c-files)
    ;; Get the .c and .o files needed to compile file
    (define-values (c-files o-files)
      (get-co-files/rec (list (build-path base exe)) '() base project-common 0))

    (logf 'debug ".c files are ~s." c-files)
    (logf 'debug ".o files are ~s." o-files)

    ;; Run the compiler - save the binary to (runtime-files-path) $name-$file-binary
    ;; if everything succeeds.
    (define-values (result messages)
      (seashell-compile-files/place
        `(,@(read-config 'compiler-flags)
          ,@(if (directory-exists? project-common) `("-I" ,(some-system-path->string project-common)) '()))
          '("-lm")
           (remove-duplicates (cons (build-path base exe) c-files))
           o-files))
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
    ;; copy the common folder to the temp dir -- for backward compatibility this term
    (when (directory-exists? project-common)
      (copy-directory/files project-common (build-path temp-dir "common")))
    ;; copy the question folder to the temp dir
    (copy-directory/files base (build-path temp-dir question-dir-name))
    ;; copy all files in the common folder to the question folder
    (for-each (lambda (apath)
                (match-define-values (_ filename _) (split-path apath))
                (copy-file apath (check-and-build-path temp-dir question-dir-name filename) #t))
              project-common-list)
    temp-dir)
  
  (define racket-temp-dir (when (equal? lang 'racket) (flatten-racket-files)))

  (define-values (result messages target)
    (match lang
      ['C (compile-c-files)]
      ['racket (values #t '() (check-and-build-path racket-temp-dir question-dir-name exe))]))

  (cond
    [(and result (empty? tests))
      (define pid (run-program target base lang #f is-cli))
      (thread
        (lambda ()
          (sync (program-wait-evt pid))
          (match lang
            ['C (delete-directory/files target #:must-exist? #f)]
            ['racket (delete-directory/files racket-temp-dir #:must-exist? #f)])))
      (values #t `#hash((pid . ,pid) (messages . ,messages) (status . "running")))]
    [result
      (define pids (map
                     (lambda (test)
                       (run-program target base lang test is-cli))
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
    (compile-and-run-project name (build-path question file-to-run) tests #f)))

 
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
              (copy-directory/files common-dir (check-and-build-path tmpdir "common")))
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


;; (read-project-settings project)
;; Reads the project settings from the project root.
;; If the file does not exist, false is returned
;; 
;; Returns:
;;   settings - the project settings, or false
(define/contract (read-project-settings project)
  (-> (and/c project-name? is-project?) (or/c #f hash-eq?))
  (define filename (read-config 'project-settings-filename))
  (cond 
    [(file-exists? (build-path (build-project-path project) filename))
     (with-input-from-file 
       (build-path (build-project-path project) filename)
       (lambda () (read)))]
    [else #f]))


;; (write-project-settings project settings)
;; Writes to the project settings in the project root.
;;
;; Arguments: settings, a hash of all the project settings
;;
;; Returns: nothing
(define/contract (write-project-settings project settings)
  (-> (and/c project-name? is-project?) hash-eq? void?)
  (with-output-to-file
    (build-path (build-project-path project) (read-config 'project-settings-filename))
    (lambda () (write settings))
    #:exists 'replace))


;; (write-project-settings/key project key val)
;; Updates the (key, val) pair in the project settings hash.
;; Equivalent to a hash-set.
;; Returns: nothing
(define/contract (write-project-settings/key project key val)
  (-> (and/c project-name? is-project?) symbol? any/c void?)
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
  (define settings-hash (read-project-settings project))
  (cond 
    [settings-hash
      (define file-to-run
        (hash-ref settings-hash 
                  (string->symbol (string-append question "-runner"))))
      (if (not (file-exists? (build-path (build-project-path project)
                                          question
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
  (if (or (string=? folder (read-config 'tests-subdirectory))
          (string=? folder (read-config 'common-subdirectory)))
    (raise (exn:project (format "You cannot set a runner file in the ~a folder." folder) 
                        (current-continuation-marks)))
    (write-project-settings/key project
                                (string->symbol (string-append question "-runner"))
                                file)))
  


