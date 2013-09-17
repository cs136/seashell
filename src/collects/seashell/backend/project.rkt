#lang racket
;; Seashell's backend server.
;; Copyright (C) 2013 The Seashell Maintainers.
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
         init-projects
         list-projects
         is-project?
         new-project
         new-project-from
         delete-project
         save-project
         exn:project?
         exn:project
         check-path
         check-and-build-path)

(require seashell/git
         seashell/seashell-config
         net/url)

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

  
;; (project-name? name) -> bool?
;; Predicate for testing if a string is a valid project name.
(define (project-name? name)
  (cond
    [(not (string? name)) #f]
    [(not (path-string? name)) #f]
    [(begin
      (define-values (base suffix _) (split-path name))
      (and (equal? base 'relative) (path-for-some-system? suffix)))
     #t]
    [else #f]))


;; (url-string? str) -> bool?
;; Predicate for testing if a string is a valid URL
(define/contract (url-string? str)
  (-> string? boolean?)
  (with-handlers
    ([url-exception? (lambda (exn) #f)])
    (string->url str)
    #t))


;; init-projects -> void?
;; Sets up the Seashell project environment
(define/contract (init-projects)
  (-> void?)
  (when (not (directory-exists? (read-config 'seashell)))
    (make-directory (read-config 'seashell))))

;; list-projects -> (listof project-name?)
;; Lists existing Seashell projects.
(define/contract (list-projects)
  (-> (listof project-name?))
  (map path->string
       (filter (compose directory-exists? (curry build-path (read-config 'seashell)))
               (directory-list (read-config 'seashell)))))

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
    (seashell-git-init (check-and-build-path (read-config 'seashell) name))))

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
    (seashell-git-clone source (check-and-build-path (read-config 'seashell) name))))

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

  ;; (recursive-delete-tree path)
  ;; Recursively deletes a directory tree.
  ;;
  ;; Arguments:
  ;;  path - path to delete.
  (define/contract (recursive-delete-tree path)
    (-> path? void?)
    (cond
      [(directory-exists? path)
       (map recursive-delete-tree
            (directory-list path #:build #t))
       (delete-directory path)]
      [else
        (delete-file path)]))
  (with-handlers
    ([exn:fail:filesystem?
       (lambda (exn)
         (raise (exn:project
                  (format "Project does not exists, or some other filesystem error occurred: ~a" (exn-message exn))
                  (current-continuation-marks))))])
    (recursive-delete-tree (check-and-build-path (read-config 'seashell) name))))

;; (save-project name)
;; Commits the current state of a project to Git.
;;
;; Arguments:
;;  name - Name of project.
;;
;; Raises:
;;  exn:project if the project does not exist,
;;  libgit2 errors if git errors happen.
(define/contract (save-project name)
  (-> project-name? void?)
  ;; Here's what we do -
  ;;  1. Grab the status of the repository.
  ;;  2. Add 'adds' to each of the files that
  ;;     have been modified and created in the working tree.
  ;;  3. Add 'deletes' to each of the files that
  ;;     have been deleted from the working tree.
  ;;  4. Run the commit.
  (define status (seashell-git-get-status (check-and-build-path (read-config 'seashell) name)))
  (define entries (seashell-git-status-entrycount status))

  (define files-add
    (map (curry seashell-git-status-path status)
         (filter
           (lambda (index)
             (define flags (seashell-git-status-flags status index))
             (or (seashell-git-flag-new? flags) (seashell-git-flag-modified? flags)))
           (build-list values entries))))

  (define files-delete
    (map (curry seashell-git-status-path status)
         (filter
           (lambda (index)
             (define flags (seashell-git-status-flags status index))
             (seashell-git-flag-deleted? flags))
           (build-list values entries))))

  (define commit (seashell-git-make-commit))
  (for-each (curry seashell-git-commit-add-file commit)
            files-add)
  (for-each (curry seashell-git-commit-delete-file commit)
            files-delete)

  (seashell-git-commit commit))

;; (is-project? name)
;; Checks if name is a project that exists
;;
;; Arguments:
;;  name - Name of project.
;; Returns: #t if it does, #f otherwise.
(define/contract (is-project? name)
  (-> project-name? boolean?)
  (directory-exists? (check-and-build-path (read-config 'seashell) name)))
