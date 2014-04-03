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
         export-project)

(require seashell/git
         seashell/log
         seashell/seashell-config
         seashell/compiler
         seashell/backend/runner
         seashell/websocket
         net/url
         file/zip)

;; global variable, which is a set of currently locked projects
(define locked-projects (make-hash))

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
  (map path->string
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
    (seashell-git-init (build-project-path name)))
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
    (seashell-git-clone source (build-project-path name)))
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
;;
;; Raises:
;;  exn:project if the project does not exist.
;;
;; Returns:
;;  #t if the project was successfully locked, #f otherwise
(define/contract (lock-project name)
  (-> (and/c project-name? is-project?) boolean?)
  (cond
    [(hash-has-key? locked-projects name) #f]
    [else (hash-set! locked-projects name #t) #t]))

;; (force-lock-project name)
;; Forcibly locks a project, even if it is already locked
;;
;; Arguments:
;;  name - Name of the project.
;;
;; Raises:
;;  exn:project if the project does not exist.
(define/contract (force-lock-project name)
  (-> (and/c project-name? is-project?) void?)
  (hash-set! locked-projects name #t))

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
  (cond
    [(hash-has-key? locked-projects name)
      (hash-remove! locked-projects name) #t]
    [else #f]))

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
  (define status (seashell-git-get-status repo))
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

  (seashell-git-commit commit message)
  (void))

;; (compile-project name)
;; Compiles a project.
;;
;; Arguments:
;;  name - Name of project.
;; Returns:
;;  List of diagnostics (error?, file, line, column, message)
;;  Error if any diagnostics have error? set.
;;
;; Raises:
;;  exn:project if project does not exist.
(define/contract (compile-project name)
  (-> project-name? (values boolean? (listof (list/c boolean? string? natural-number/c natural-number/c string?))))
  (when (not (is-project? name))
    (raise (exn:project (format "Project ~a does not exist!" name)
                        (current-continuation-marks))))
  ;; TODO: Other languages? (C++, maybe?)
  ;; Get *.c files in project.
  (define c-files
    (filter (lambda (file)
              (equal? (filename-extension file) #"c"))
            (directory-list (build-project-path name) #:build? #t)))
  (define o-files
    (filter (lambda (file)
              (equal? (filename-extension file) #"o"))
            (directory-list (build-project-path name) #:build? #t)))
  ;; Run the compiler - save the binary to .seashell/${name}-binary,
  ;; if everything succeeds.
  (define-values (result messages)
    (seashell-compile-files/place '("-Wall" "-gdwarf-4" "-O0") '("-lm") c-files o-files))
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
                          (path->string (file-name-from-path key))
                          (seashell-diagnostic-line diagnostic)
                          (seashell-diagnostic-column diagnostic)
                          (seashell-diagnostic-message diagnostic)))
                  diagnostics))))))

;; (run-project name)
;; Runs a project
;;
;; Arguments:
;;  name - Name of project.
;; Returns:
;;  pid - Process ID (used as unique identifier for process)
(define/contract (run-project name)
  (-> project-name? integer?)
  (when (not (is-project? name))
    (raise (exn:project (format "Project ~a does not exist!" name)
                        (current-continuation-marks))))
  ;; TODO: Racket mode.
  (define output-path (check-and-build-path (runtime-files-path) (format "~a-binary" name)))
  (run-program output-path
               (check-and-build-path (build-project-path name))))

;; (export-project name) -> bytes?
;; Exports a project to a ZIP file.
;;
;; Arguments:
;;  name - Name of project.
;; Returns:
;;  zip - ZIP file as a bytestring.
(define/contract (export-project name)
  (-> project-name? bytes?)
  (when (not (is-project? name))
    (raise (exn:project (format "Project ~a does not exist!" name)
                        (current-continuation-marks))))
  (parameterize
    ([current-directory (build-project-path name)])
    (with-output-to-bytes
      (thunk
        (zip->output (pathlist-closure (directory-list)))))))
