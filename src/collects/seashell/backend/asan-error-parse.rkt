#lang typed/racket

;; Use the function "asan-error-parse" to reads the asan error message
;; and procude a parsed structure
(provide asan-rewrite)

(struct StackFrame ([frame : Integer]
                    [file : String]
                    [ln : Integer]
                    [func : String])
  #:transparent)
(define-type ErrorComponent (U String StackFrame))
(define-type ASANError (Listof ErrorComponent))

;; wrapper function
(: asan-rewrite (Bytes -> Bytes))
(define (asan-rewrite x)
  (cond [(zero? (bytes-length x)) x]
        [else (string->bytes/utf-8
                (format "\n~a\n" 
                        (ASANError->string 
                          (asan-error-parse (bytes->string/utf-8 x)))))]))

;; rewrite ASANError in a friendly way
(: ASANError->string (ASANError -> String))
(define (ASANError->string x)
  (string-join 
    (filter-map 
      (lambda ([x : ErrorComponent])
        (match x
          [(pregexp #px"^=+\\d+=+ERROR: AddressSanitizer: SEGV") "Error: Segmentation fault"]
          [(pregexp #px"^=+\\d+=+ERROR: AddressSanitizer: stack-buffer-overflow") "Error: Stack overflows"]
          [(pregexp #px"^=+\\d+=+ERROR: AddressSanitizer: heap-buffer-overflow") "Error: Heap overflows"]
          [(pregexp #px"^=+\\d+=+ERROR: AddressSanitizer: global-buffer-overflow") "Error: Global buffer overflows"]
          [(pregexp #px"^=+\\d+=+ERROR: AddressSanitizer: heap-use-after-free") "Error: Using variables after free"]
          [(pregexp #px"^=+\\d+=+ERROR: LeakSanitizer: detected memory leaks") "Error: Memory leaks"]
          [(pregexp #px"^(.+)from:" (list _ x)) x] ;; trim "from" to make the sentense more fluent
          [(? string?) x]
          [(StackFrame frame (? student-file-path? file) ln func)
           (format "   in function call ~v, module ~v, line ~a" func (path->string (assert (file-name-from-path file))) ln)]
          [(StackFrame frame _ _ (? interceptor? func))
           (format "   in function call ~v" (string-trim func "__interceptor_"))]
          [else #f]))
      x)
    "\n"))


(: student-file-path? (String -> Boolean))
(define (student-file-path? path)
  (regexp-match? #px"/home/y667li/.seashell/projects/" path))

(: interceptor? (String -> Boolean))
(define (interceptor? func)
  (regexp-match? #px"__interceptor_" func))

;; The main parser function. Use the error message as the argument.
(: asan-error-parse (String -> ASANError))
(define (asan-error-parse contents)
  (define lines : (Listof String) 
    (filter-map (lambda ([x : String])
                  (match (string-trim x)
                    ["" #f]
                    [x x]))
                (string-split contents "\n")))
  (filter-map 
    (lambda ([x : String])
      (cond [(findf (compose not false?)
                    (map (lambda ([f : (String -> (U False ErrorComponent))]) 
                           (f x))
                         (try-list))) => identity]
            [else (eprintf "# Ignored line: \"~a\"\n" x) #f]))
    lines))

(define (inspect x) (eprintf "~v\n" x) x)

(: try-list (-> (Listof (String -> (U False ErrorComponent)))))
(define (try-list) 
  (list extract-stackframe accept-str))


;; Do not rewrite string in this function. You should only add match cases
;;  that produce #f here. Rewrite strings in ASANError->string instead.
(: accept-str (String -> (U False String)))
(define (accept-str str)
  (match str
    [(pregexp #px"^=+$") #f]
    [(pregexp #px"^SUMMARY") #f]
    [(pregexp #px"^AddressSanitizer can not provide additional info.") #f]
    [(pregexp #px"^=+\\d+=+ABORTING") #f]
    [x x]))

(: extract-stackframe (String -> (U False StackFrame)))
(define (extract-stackframe line)
  (define px-frame "\"frame\"\\:\\s+(\\d+)")
  (define px-module "\"module\"\\:\\s+\"[^\"]+\"")
  (define px-offset "\"offset\"\\:\\s+\"[^\"]+\"")
  (define px-function "\"function\"\\:\\s+\"([^\"]+)\"")
  (define px-function-offset "\"function_offset\"\\:\\s+\"[^\"]+\"")
  (define px-file "\"file\"\\:\\s+\"([^\"]+)\"")
  (define px-line "\"line\"\\:\\s+(\\d+)")
  (define px-column "\"column\"\\:\\s+\\d+")
  (define px (string-join (list px-frame 
                                px-module
                                px-offset
                                px-function
                                px-function-offset
                                px-file
                                px-line
                                px-column
                                ) 
                          ",\\s+"))
  (match (regexp-match (pregexp px) line)
    [(list _ 
           (? string? frame) 
           (? string? func)
           (? string? file)
           (? string? ln))
     (StackFrame (str->int frame)
                 file
                 (str->int ln)
                 func)]
    [else #f]))

(: str->int (String -> Integer))
(define (str->int str)
  (match (string->number str)
    [(? exact-integer? x) x]
    [_ (error 'ERROR "str->int: expects an integer but get ~s\n" str)]))

