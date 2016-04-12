#lang typed/racket

;; Use the function "asan-error-parse" to reads the asan error message
;; and procude a parsed structure
(provide format-asan-error)

(struct StackFrame ([frame : Integer]
                    [file : String]
                    [ln : Integer]
                    [func : String])
  #:transparent)
(define-type ErrorComponent (U String StackFrame))
(define-type ASANError (Listof ErrorComponent))

(: format-asan-error (String -> String))
(define (format-asan-error x)
  (ASANError->string (asan-error-parse x)))

;; format ASANError in a friendly manner
(: ASANError->string (ASANError -> String))
(define (ASANError->string x)
  (string-join 
    (map (lambda ([x : ErrorComponent])
           (match x
             [(? string?) x]
             [(StackFrame frame file ln func)
              (format "frame ~a, file \"~a\", line ~a, function \"~a\"" frame file ln func)]))
         x) 
    "\n"
    #:after-last "\n"))

;; The main parser function. Use the error message as the argument.
(: asan-error-parse (String -> ASANError))
(define (asan-error-parse contents)
  (define lines : (Listof String) 
    (filter-map (lambda ([x : String])
                  (match (string-trim x)
                    ["" #f]
                    [x x]))
                (string-split contents "\n")))
  (when (< (length lines) 2)
    (error 'ParseError "Input message is too short.\n Here is an example of an expected input:\n 
           =================================================================
           ==33482==ERROR: LeakSanitizer: detected memory leaks
           Direct leak of 144 byte(s) in 9 object(s) allocated from:
           {\"frame\": 0, \"module\": \"../A10-merge.c-g158-binary\", \"offset\": \"0x4b0240\", \"function\": \"malloc\", \"function_offset\": \"0x0\", \"file\": \"../asan_malloc_linux.cc\", \"line\": 40, \"column\": 0}
           {\"frame\": 1, \"module\": \"../A10-merge.c-g158-binary\", \"offset\": \"0x4d2194\", \"function\": \"read_list\", \"function_offset\": \"0x0\", \"file\": \"../merge_tester.c\", \"line\": 21, \"column\": 31}
           {\"frame\": 2, \"module\": \"../A10-merge.c-g158-binary\", \"offset\": \"0x4d262b\", \"function\": \"main\", \"function_offset\": \"0x0\", \"file\": \"../merge_tester.c\", \"line\": 43, \"column\": 3}
           {\"frame\": 3, \"module\": \"../libc.so.6\", \"offset\": \"0x2176c\", \"function\": \"__libc_start_main\", \"function_offset\": \"0x0\", \"file\": \"../libc-start.c\", \"line\": 226, \"column\": 0}
           SUMMARY: AddressSanitizer: 144 byte(s) leaked in 9 allocation(s)
           \n"))
  
  (filter-map 
    (lambda ([x : String])
      (cond [(findf (compose not false?)
                    (map (lambda ([f : (String -> (U False ErrorComponent))]) 
                           (f x))
                         (try-list))) => identity]
            [else (eprintf "Ignored line \"~a\"\n" x) #f]))
    lines))

(define (inspect x) (eprintf "~v\n" x) x)

(: try-list (-> (Listof (String -> (U False ErrorComponent)))))
(define (try-list) 
  (list extract-stackframe accept-str))

(: accept-str (String -> (U False String)))
(define (accept-str str)
  (match str
    [(pregexp #px"^=+$") #f]
    [(pregexp #px"=+\\d+=+(.+)" (list _ x)) x]
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


