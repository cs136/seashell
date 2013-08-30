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
         exn:project)

;; (project-name? name) -> bool?
;; Predicate for testing if a string is a valid project name.
(define project-name? path-string?)

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
    (make-directory seashell)))

;; list-projects -> (listof project-name?)
;; Lists existing Seashell projects.
(define/contract (list-projects)
  (-> (listof project-name?))
  (map path->string
       (filter (compose directory-exists? (curry build-path (read-config 'seashell)))
               (directory-list seashell))))

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
    (make-directory (build-path (read-config 'seashell) name))
    ;; TODO - add a libgit2 call here to create a new git repository
    ;; FFI to git_repository_init
    ))

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
(define/contract (new-project-from-name name source)
  (-> project-name? (or/c project-name? url-string?) void?)
  (with-handlers
    ([exn:fail:filesystem?
       (lambda (exn)
         (raise (exn:project
                  (format "Project already exists, or some other filesystem error occurred: ~a" (exn-message exn))
                  (current-continuation-marks))))])
    (make-directory (build-path (read-config 'seashell) name))
    ;; TODO - add a libgit2 call to clone the existing git repository
    ;; FFI to git_clone
    ))

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
    (recursive-delete-tree (build-path (read-config 'seashell) name))))

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
  ;; TODO - actually write this function.
  (void))

;; (is-project? name)
;; Checks if name is a project that exists
;;
;; Arguments:
;;  name - Name of project.
;; Returns: #t if it does, #f otherwise.
(define/contract (is-project? name)
  (-> project-name? boolean?)
  ;; TODO - probably should also query git.
  (directory-exists? (build-path (read-config 'seashell) name)))
