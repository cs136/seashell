#lang typed/racket

(require/typed racket/random
  [crypto-random-bytes (-> Integer Bytes)])
(require typed/json)
(require typed/db)
(require typed/db/sqlite3)
(require "../db/database.rkt")

;; just setting something up so I can test for now
(unless (file-exists? "test.db")
  (disconnect (sqlite3-connect #:database "test.db" #:mode 'create)))

(define db (make-object sync-database% 'memory))

;; What I'm thinking:
;;
;; Tables:
;; contents: id, project_id, file_id, contents, time
;; files: id, project_id, name, contents_id, flags
;; projects: id, name, settings, last_used

(: get-uuid (-> String))
(define (get-uuid)
  ;; TODO change this to actual UUIDs
  (bytes->string/utf-8 (crypto-random-bytes 16)))

;; TODO support adding project from template
(: new-project (-> String String))
(define (new-project name)
  (define proj-id (get-uuid))
  (send db apply-create "projects" proj-id
    `#hasheq((name . ,name)
             (settings . #hasheq())
             (last_used . ,(current-milliseconds))))
  proj-id)

(: delete-project (-> String Void))
(define (delete-project id)
  (send db apply-delete "projects" id))

(: new-file (-> String String (U String False) Integer String))
(define (new-file pid name contents flags)
  (define file-id (get-uuid))
  (define contents-id (get-uuid))
  (send db write-transaction (thunk
    (send db apply-create "contents" contents-id
      `#hasheq((project_id . ,pid)
               (file_id . ,file-id)
               (contents . ,contents)
               (time . ,(current-milliseconds))))
    (send db apply-create "files" file-id
      `#hasheq((project_id . ,pid)
               (name . ,name)
               (contents_id . ,contents-id)
               (flags . ,flags)))))
  contents-id)
