#lang racket
;; Seashell's Clang interface.
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
(struct seashell-diagnostic (error? file line column message) #:prefab)

(require seashell/compiler/ffi
         seashell/seashell-config)

(provide
  seashell-compile-files
  seashell-diagnostic
  seashell-compiler-place-main
  seashell-compile-files/place)

;; (seashell-compile-files cflags ldflags source)
;; Invokes the internal compiler and external linker to create
;; an executable based on the source files.
;;
;; Arguments:
;;  cflags - List of flags to pass to the internal compiler.
;;  ldflags - List of flags to pass to the system compiler to
;;            finish the assembly and linking passes.
;;  source - List of source paths to compile.
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
(define/contract (seashell-compile-files cflags ldflags sources)
  (-> (listof string?) (listof string?) (listof path?)
      (values (or/c bytes? false?) (hash/c path? (listof seashell-diagnostic?))))

  ;; Set up the compiler instance.
  (define compiler (seashell_compiler_make))
  (define file-vec (list->vector sources))
  (seashell_compiler_clear_files compiler)
  (seashell_compiler_clear_compile_flags compiler)
  (for-each ((curry seashell_compiler_add_compile_flag) compiler) cflags)
  (for-each ((curry seashell_compiler_add_file) compiler) (map path->string sources))

  ;; Run the compiler + intermediate linkage step.
  (define compiler-res (seashell_compiler_run compiler))
  ;; Grab the results of running the intermediate code generation step.
  (define intermediate-linker-diags (seashell_compiler_get_linker_messages compiler))
  ;; Generate our diagnostics:
  (define compiler-diags
    (foldl
      (lambda (message table)
        (if (not (equal? "" (seashell-diagnostic-message (cdr message))))
          (hash-set table (car message)
                    (cons (cdr message)
                          (hash-ref table (car message) '())))
          table))
      (make-immutable-hash)
      (list*
        `(,(string->path "intermediate-link-result") . ,(seashell-diagnostic (not (zero? compiler-res)) "" 0 0 intermediate-linker-diags))
        (for*/list ([i (in-range 0 (length sources))]
                    [j (in-range 0 (seashell_compiler_get_diagnostic_count compiler i))])
          `(,(vector-ref file-vec i) .
             ,(seashell-diagnostic (seashell_compiler_get_diagnostic_error compiler i j)
                                   (seashell_compiler_get_diagnostic_file compiler i j)
                                   (seashell_compiler_get_diagnostic_line compiler i j)
                                   (seashell_compiler_get_diagnostic_column compiler i j)
                                   (seashell_compiler_get_diagnostic_message compiler i j)))))))

  (cond
    [(zero? compiler-res)
      ;; Invoke the final link step on the assembly file - if it exists.
      ;; It's a good thing GNU as supports pipes.
      (define object (seashell_compiler_get_object compiler))
      (define-values (linker linker-output linker-input linker-error)
        (apply subprocess #f #f #f (read-config 'system-compiler)
               (append ldflags
                       (list "-x" "assembler" "-pipe" "-o" "/dev/stdout" "/dev/stdin"))))
      ;; Close unused port.
      (write-bytes object linker-input)
      (close-output-port linker-input)
      ;; Read result from linker:
      (define linker-result (port->bytes linker-output))
      (define linker-messages (port->string linker-error))
      (close-input-port linker-output)
      (close-input-port linker-error)
      ;; Get the linker termination code
      (define linker-res (subprocess-status (sync linker)))

      ;; Create the final diagnostics table:
      (define diags
        (foldl
          (lambda (message table)
            (if (not (equal? "" (cdr message)))
              (hash-set table (car message)
                        (cons (seashell-diagnostic (not (zero? linker-res)) "" 0 0 (cdr message))
                              (hash-ref table (car message) '())))
              table))
          compiler-diags
          ;; Create list of linker diagnostics:
          (list*
            (map (curry cons (string->path "final-link-result"))
                 (string-split linker-messages #px"\n")))))
  
      (if (and (zero? compiler-res) (zero? linker-res))
        (values linker-result diags)
        (values #f diags))]
    [else
      (values #f compiler-diags)]))

;; (seashell-compiler-place-main pch)
;; Main function for a place that reads, from its channel:
;;  cflags, ldflags, sources - as above.
;; And writes:
;;  Result of running seashell-compile-files onto its channel.
;;
;; Arguments:
;;  pch - Place channel.
(define (seashell-compiler-place-main pch)
  (define cflags (place-channel-get pch))
  (define ldflags (place-channel-get pch))
  (define sources (place-channel-get pch))

  (define-values (object diags) (seashell-compile-files cflags ldflags sources))
  (cond
    [(bytes? object)
      (define result (make-shared-bytes (bytes-length object)))
      (bytes-copy! result 0 object 0)
      (place-channel-put pch result)]
    [else
      (place-channel-put pch object)])
  (place-channel-put pch diags))

;; (seashell-compile-files/place cflags ldflags sources)
;; Like seashell-compile-files, but invokes the compilation process
;; in a separate place.  This is used to enable parallelism
;; when dealing with the FFI.
(define (seashell-compile-files/place cflags ldflags sources)
  (define result (dynamic-place 'seashell/compiler/seashell-compiler 'seashell-compiler-place-main))
  (place-channel-put result cflags)
  (place-channel-put result ldflags)
  (place-channel-put result sources)
  (define object (place-channel-get result))
  (define diags (place-channel-get result))
  (values object diags))
