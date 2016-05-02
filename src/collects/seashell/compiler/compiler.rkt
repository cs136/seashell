#lang typed/racket
;; Seashell's Clang interface.
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
(require seashell/compiler/ffi
         (submod seashell/seashell-config typed))
(require/typed racket/base
               [file-position (case-lambda
                                (-> Port Exact-Nonnegative-Integer)
                                (-> Port (U Integer EOF) Void))])
(provide
 Seashell-Diagnostic
 Seashell-Diagnostic-Table
 seashell-compile-files
 (struct-out seashell-diagnostic))

(module untyped racket/base
  (require racket/serialize)
  (provide (all-defined-out))
  (serializable-struct seashell-diagnostic (error? file line column message) #:transparent))
(require/typed (submod "." untyped)
               [#:struct seashell-diagnostic ([error? : Boolean] [file : String] [line : Index] [column : Index] [message : String])
                #:type-name Seashell-Diagnostic])
(define-type Seashell-Diagnostic-Table (HashTable Path (Listof Seashell-Diagnostic)))

;; (seashell-compile-files cflags ldflags source)
;; Invokes the internal compiler and external linker to create
;; an executable based on the source files.
;;
;; Arguments:
;;  cflags - List of flags to pass to the compiler.
;;  ldflags - List of flags to pass to the linker.
;;  source - List of source paths to compile.
;;  objects - List of object paths to compile.
;; Returns:
;;  (values #f (hash/c path? (listof seashell-diagnostic?))) - On error,
;;    returns no binary and the list of messages produced.
;;  (values bytes? (hash/c path? (listof seashell-diagnostic?))) - On success,
;;    returns the ELF binary as a bytestring and the list of messages produced.
;;
;; Notes:
;;  This function ought not to produce any exceptions - otherwise,
;;  things may go south if this is running in a place.  It might be
;;  worthwhile installing an exception handler in the place main
;;  function to deal with this, though.
(: seashell-compile-files (-> (Listof String) (Listof String) (Listof Path) (Listof Path)
                              (Values (U Bytes False) Seashell-Diagnostic-Table)))
(define (seashell-compile-files user-cflags user-ldflags sources objects)
  ;; Check that we're not compiling an empty set of sources.
  ;; Bad things happen.
  (cond
    [(empty? sources)
     (values #f (hash (string->path "final-link-result")
                      (list (seashell-diagnostic #t "" 0 0 "No files passed to compiler!"))))]
    [else
     ;; Set up the proper flags.  Headers are taken care of in compiler.cc.
     ;; Might be worth writing stripped down C standard headers.
     (define cflags
       (list*
        "-fsanitize=address"
        user-cflags))

     ;; Set up the compiler instance.
     (define compiler (seashell_compiler_make))
     (define file-vec (list->vector sources))
     (seashell_compiler_clear_files compiler)
     (seashell_compiler_clear_compile_flags compiler)
     (for-each (lambda ([flag : String]) (seashell_compiler_add_compile_flag compiler flag)) cflags)
     (for-each (lambda ([file : String]) (seashell_compiler_add_file compiler file)) (map some-system-path->string sources))

     ;; Run the compiler + intermediate linkage step.
     (define compiler-res (seashell_compiler_run compiler))
     ;; Grab the results of running the intermediate code generation step.
     (define intermediate-linker-diags (seashell_compiler_get_linker_messages compiler))
     ;; Generate our diagnostics:
     (define compiler-diags
       (foldl
        (lambda ([message : (Pair Path Seashell-Diagnostic)]
                 [table : (HashTable Path (Listof Seashell-Diagnostic))])
          (if (not (equal? "" (seashell-diagnostic-message (cdr message))))
              (hash-set table (car message)
                        (cons (cdr message)
                              (hash-ref table (car message) (lambda () '()))))
              table))
        #{(make-immutable-hash) :: (HashTable Path (Listof Seashell-Diagnostic))}
        (list*
         `(,(string->path "intermediate-link-result") . ,(seashell-diagnostic (not (zero? compiler-res)) "" 0 0 intermediate-linker-diags))
         (for*/list : (Listof (Pair Path Seashell-Diagnostic))
                    ([i : Index #{(in-range (length sources)) :: (Sequenceof Index)}]
                     [j : Index #{(in-range (seashell_compiler_get_diagnostic_count compiler i)) :: (Sequenceof Index)}])
           `(,(vector-ref file-vec i) .
                                      ,(seashell-diagnostic (seashell_compiler_get_diagnostic_error compiler i j)
                                                            (seashell_compiler_get_diagnostic_file compiler i j)
                                                            (seashell_compiler_get_diagnostic_line compiler i j)
                                                            (seashell_compiler_get_diagnostic_column compiler i j)
                                                            (seashell_compiler_get_diagnostic_message compiler i j)))))))
     ;; Grab the object - note that it may not exist yet.
     (define object (seashell_compiler_get_object compiler))
     (cond
       [(and (bytes? object) (zero? compiler-res))
        ;; Set up the linker flags.
        (define ldflags user-ldflags)
        ;; Write it to a temporary file, invoke cc -Wl,$ldflags -o <result_file> <object_file>
        (define object-file (make-temporary-file "seashell-object-~a.o"))
        (define result-file (make-temporary-file "seashell-result-~a"))
        (with-output-to-file object-file
          (lambda () (write-bytes object))
          #:exists 'truncate)
        (: append-linker-flag (-> String String))
        (define (append-linker-flag flag)
          (string-append (read-config-string 'linker-flag-prefix) flag))
        (define-values (linker linker-output linker-input linker-error)
          (apply subprocess #f #f #f (read-config-path 'system-linker)
                 `("-o" ,(some-system-path->string result-file)
                   ,(some-system-path->string object-file)
                   ,@(map some-system-path->string objects)
                   "-fsanitize=address"
                   ,@(map
                       append-linker-flag
                       (list "--whole-archive"
                             (some-system-path->string (build-path (read-config-path 'seashell-runtime-library)))
                             "--no-whole-archive"))
                   ,@(map append-linker-flag ldflags))))
        ;; Close unused port.
        (close-output-port linker-input)
        (close-input-port linker-output)
        ;; Read messages from linker:
        (define linker-messages (port->string linker-error))
        (close-input-port linker-error)
        ;; Get the linker termination code
        (define linker-res (subprocess-status (sync linker)))
        ;; Remove the object file.
        (delete-file object-file)

        ;; Read the result:
        (define linker-result
          (cond
            [(equal? 0 linker-res)
              (call-with-input-file
                result-file
                (lambda ([port : Input-Port])
                  (file-position port eof)
                  (define size (file-position port))
                  (file-position port 0)
                  (define result (make-shared-bytes size))
                  (read-bytes! result port)
                  result))]
            [else
              #f]))
        ;; Fix binaries taking up all the space in /var/tmp
        (delete-directory/files result-file #:must-exist? #f)
        ;; Create the final diagnostics table:
        (define diags
          (foldl
           (lambda ([message : (Pairof Path String)]
                    [table : (HashTable Path (Listof Seashell-Diagnostic))])
             (if (not (equal? "" (cdr message)))
                 (hash-set table (car message)
                           (cons (seashell-diagnostic (not (equal? 0 linker-res)) "" 0 0 (cdr message))
                                 (hash-ref table (car message) (lambda () '()))))
                 table))
           compiler-diags
           ;; Create list of linker diagnostics:
           (list*
            (map (lambda ([message : String])
                   (cons (string->path "final-link-result") message))
                 (string-split linker-messages #px"\n")))))

        (if (and (zero? compiler-res) (equal? 0 linker-res))
            (values linker-result diags)
            (values #f diags))]
       [else
        (values #f compiler-diags)])]))
