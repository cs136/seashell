#lang typed/racket/no-check
;; TODO: Change above ^^ to typed/racket when match issue fixed.
;; Seashell
;; Copyright (C) 2012-2014 The Seashell Maintainers
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
(require seashell/log
         (submod seashell/seashell-config typed)
         typed/json)
(require/typed seashell/backend/project
               [list-projects (-> (Listof (List String Number)))]
               [#:struct (exn:project exn:fail:user) ()])
(require/typed seashell/backend/files
               [new-file (-> String Path-String Bytes (U 'raw 'url) Boolean String)]
               [remove-file (-> String Path-String Void)]
               [read-file (-> String Path-String (Values Bytes String))]
               [write-file (->* (String Path-String String) ((U False String)) String)]
               [list-files (-> (Listof (List String Boolean Number String)))]
               [#:struct (exn:project:file exn:project) ()]
               [#:struct (exn:project:file:checksum exn:project:file) ()])

(: sync-offline-changes (-> JSExpr JSExpr))
(define (sync-offline-changes changeset)
  ;; TODO: Grab global lock to protect against _all_ operations.
  ;; Do not want other operations modifying state that the offline engine needs to work with.

  (match-define (hash-table ('projects #{their-projects : (Listof String)})
                            ('files #{their-files : (Listof JSExpr)}) 
                            ('changes #{their-changes : (Listof JSExpr)}))
    (cast changeset (HashTable Symbol JSExpr)))
  ;; NOTE: We do not expect new projects, hence it is safe to apply the changes first
  ;; before looking at projects/files.

  ;; Apply changes, collect conflicts. 
  (define
    conflicts
    (foldl
      (lambda ([change : JSExpr] [conflicts : (Listof (List String Path-String String))])
        : (Listof (List String Path-String String))
        (match (cast change (HashTable Symbol JSExpr))
          [(hash-table
             ('type "deleteFile")
             ('project #{project : String})
             ('file #{file : Path-String})
             ('checksum #{checksum : String}))
           ;; TODO Delete only if checksum matches.
           (remove-file project file)
           conflicts]
          [(hash-table
             ('type "editFile")
             ('project #{project : String})
             ('file #{file : String})
             ('contents #{contents : String})
             ('checksum #{checksum : (U String False)}))
           (with-handlers
             ([exn:project:file:checksum?
                (lambda ([exn : exn])
                  (cons (list project file contents) conflicts))])
             (cond
               [checksum
                 (write-file project file (string->bytes/utf-8 contents) checksum)]
               [else
                 (new-file project file (string->bytes/utf-8 contents) 'raw #f)])
             conflicts)]))
      '()
      their-changes))
  ;; Collect list of new projects.
  (define our-projects (list-projects))
  (define new-projects (remove* our-projects their-projects))
  ;; Resolve conflicts (add .conflict for each file)
  ;; Collect list of changes.
  (define our-files
    (foldl
      (lambda ([project : String] [files : (Listof JSExpr)])
        )
      '()
      our-projects))
  (define new-files (remove* our-files their-files))
  (define conflict-files #f)
  ;; TODO settings?
  '())
