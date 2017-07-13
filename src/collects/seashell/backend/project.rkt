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

(provide url-string?
         init-projects
         compile-and-run-project
         marmoset-submit
         marmoset-test-results
         archive-projects)

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
;;;  exn:project if the project already exists.
;(define/contract (new-project-from name source)
;  (-> string? (or/c url-string? path-string?) void?)
;  (with-handlers
;    ([exn:fail:filesystem?
;       (lambda (exn)
;         (raise (exn:project
;                  (format "Project already exists, or some other filesystem error occurred: ~a" (exn-message exn))
;                  (current-continuation-marks))))])
;    (call-with-write-lock (thunk
;      (cond
;        [(or (path-string? source) (url-string? source))
;          (make-directory (build-project-path name))
;          (with-handlers
;            ([exn:fail? (lambda (exn)
;              (delete-directory/files (build-project-path name) #:must-exist? #f)
;              (raise exn))])
;            (parameterize ([current-directory (build-project-path name)])
;              (call-with-template source
;                (lambda (port) (unzip port (make-filesystem-entry-reader #:strip-count 1))))))]
;        [(project-name? source)
;         (copy-directory/files (build-project-path source)
;                               (build-project-path name))]))))
;  (void))

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
  (when (or (not full-path)
            (and full-path (not (directory-exists? name))))
    (raise (exn:project (format "Project ~a does not exist!" name)
                        (current-continuation-marks))))

  (define project-base name)
  (define project-base-str (path->string (path->complete-path project-base)))
  (define project-common (if full-path
    (build-path project-base (read-config 'common-subdirectory))
    (build-path project-base (read-config 'common-subdirectory))))
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
      [_ (error "You can only run .c or .rkt files!")]))
  ;; Base path, and basename of the file being run
  (match-define-values (base exe _)
    (split-path (build-path project-base file)))
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
      ['racket (values #t '() (build-path racket-target-dir exe))]))

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
  (-> string? string? string? (or/c #f path-string?) void?)
  (void))
;  (define tmpzip #f)
;  (define tmpdir #f)
;
;  (dynamic-wind
;    (lambda ()
;      (set! tmpzip (make-temporary-file "seashell-marmoset-zip-~a"))
;      (set! tmpdir (make-temporary-file "seashell-marmoset-build-~a"
;                                         'directory)))
;    (lambda ()
;      (cond
;        ;; Two cases - either we're submitting a subdirectory...
;        [subdirectory
;          ;; Here's what we do to ensure correct linkage.
;          ;;
;          ;; common/filesA*                 filesA
;          ;; question/filesB*          -->  filesB
;          ;; question/tests/tests*          tests/
;
;          ;; TODO what to do with duplicate file names in common/ and in question/?
;          ;; Right now we toss an exception.
;          (define project-dir 
;            (build-project-path project))
;          (define question-dir
;            (build-path project-dir subdirectory))
;          (define common-dir
;            (build-path project-dir (read-config 'common-subdirectory)))
;          (parameterize ([current-directory tmpdir])
;            (define (copy-from! base include-hidden [dest #f])
;              (fold-files
;                (lambda (path type _)
;                  (define file (last (explode-path path)))
;                  (cond
;                    [(equal? path base) (values #t #t)]
;                    ;; do not submit hidden files to Marmoset
;                    [(and (not include-hidden) (string-prefix? (path->string file) "."))
;                      (values #t #t)]
;                    [else
;                      (match
;                        type
;                        ['dir
;                         (make-directory
;                           (find-relative-path base path))
;                         (values #t #t)]
;                        ['file
;                         (copy-file path
;                                    (if dest (build-path dest file)
;                                             (find-relative-path base path)))
;                         (values #t #t)]
;                        [_ (values #t #t)])]))
;                #t
;                base))
;            
;            (copy-from! question-dir #f)
;            (when (directory-exists? common-dir)
;              (define compath (build-path tmpdir "common"))
;              (make-directory compath)
;              (copy-from! common-dir #f compath))
;            (with-output-to-file
;              tmpzip
;              (lambda () (zip->output (pathlist-closure (directory-list))))
;              #:exists 'truncate))]
;        ;; Or we're submitting the entire project.
;        [else
;          (with-output-to-file
;            tmpzip
;            (lambda () (write-bytes (export-project project)))
;            #:exists 'truncate)])
;
;      ;; Launch the submit process.
;      (define-values (proc out in err)
;        (subprocess #f #f #f (read-config 'submit-tool) course assn tmpzip))
;
;      ;; Wait until it's done.
;      (subprocess-wait proc)
;      (define stderr-output (port->string err))
;      (define stdout-output (port->string out))
;      (define exit-status (subprocess-status proc))
;      (close-output-port in)
;      (close-input-port out)
;      (close-input-port err)
;      
;      ;; Report errors
;      (unless (zero? exit-status)
;        (raise (exn:project (format "Could not submit project - marmoset_submit returned ~a: (~a) (~a)"
;                                    exit-status
;                                    stderr-output stdout-output)
;                            (current-continuation-marks)))))
;    (lambda ()
;      (delete-directory/files tmpzip #:must-exist? #f)
;      (delete-directory/files tmpdir #:must-exist? #f))))

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
  (define dir-path (build-path (read-config 'seashell) "archives"
    (if archive-name archive-name (number->string (current-seconds)))))
  (define arch-root (build-path (read-config 'seashell) "archives"))
  (define proj-root (build-path (read-config 'seashell) "projects"))
  (unless (directory-exists? arch-root)
    (make-directory arch-root))
  (rename-file-or-directory proj-root dir-path)
  (make-directory proj-root))
