#lang typed/racket

(require/typed json [jsexpr->string (JSExpr -> String)]
               [string->jsexpr (String -> JSExpr)]
               [jsexpr? (Any -> Boolean)])
(require/typed racket/string [string-prefix? (String String -> Boolean)])
(require/typed racket/hash [hash-union (JSHash JSHash -> JSHash)])
(require/typed racket/base [regexp-match (PRegexp String -> (U False (Listof String)))]
               [string->number (String -> Real)])

(require (submod seashell/seashell-config typed))
;; use these two lines below instead of the one above to use the driver
;(: read-config-path (Any -> Path-String))
;(define (read-config-path x) (build-path (find-system-path 'home-dir) ".seashell"))

(provide asan->json)


;; JSHash, JSList, and JSExpr are types used to store JSON data.
;; They just use Racket's hashes and lists. These types exist
;; because type checking with hashes is a pain (lots of
;; polymorphic type errors).
(define-type JSHash (HashTable Symbol JSExpr))
(define-type JSList (Listof JSExpr))
(define-type JSExpr (U String Integer JSList JSHash))

(: jsexpr-add (JSHash Symbol JSExpr -> JSHash))
(define (jsexpr-add jsexpr key value)
  ((inst hash-set Symbol JSExpr) jsexpr key value))

(: jsexpr ((Listof (List Symbol JSExpr)) -> JSHash))
(define (jsexpr assoc-list)
  (cond [(empty? assoc-list) (hash)]
        [else (jsexpr-add (jsexpr (rest assoc-list))
                          (first (first assoc-list))
                          (second (first assoc-list)))]))

(: jsexpr-ref (JSHash Symbol -> (U JSExpr False)))
(define (jsexpr-ref jsexpr key)
  ((inst hash-ref Symbol JSExpr False) jsexpr key #f))



;;
;; ASAN output is typically divided into sections separated by blank lines.
;; For example, a program with multiple memory leaks will have ASAN ouput
;; consisting of a section for each leak, and each section will have a size of
;; the leak and the call stack.
;;
;; A SectionParser consumes a list of lines representing part of ASAN output,
;; and it'll try to parse the next several lines (possibly only one line) into
;; a section. Each SectionParser will check one type of section (ex. you can have
;; a SectionParser that checks for memory-leak sections, and another that checks
;; for stack overflow sections).
;;
;; A SectionParser also consumes the other arguments of asan-parser (error-type,
;; call-stacks, and extra-info) if you need to access that information.
;;
;; A SectionParser returns a SectionData which contains data of the parsed section.
;; Each field in a SectionData can be #f, to indicate that the parsed section
;; doesn't change that field.
;;
;; Meaning of fields in SectionData structure:
;;
;;  * error-type: A short string that indicates the error type, ex. "memory-leak"
;;                or "stack-buffer-overflow".
;;  * call-stack: A hash containing a list of frames ('framelist'), and a 'misc'
;;                dictionary containing extra info about the framelist. This
;;                call-stack will be added to the list of call-stacks that
;;                asan-parser maintains.
;;  * extra-info: A dictionary containing info that's not related to any framelist.
;;                It's similar to the 'misc' dictionary in the call-stack, but
;;                the data here isn't associated with any particular frame list.
;;  * lines-left: A list of the lines that still have to be parsed after this
;;                section was parsed.
;;
(define-type SectionParser ((Listof String) String JSList JSHash -> SectionData))
(define-type ParserList (Listof SectionParser))
(struct SectionData ([error-type : (U False String)]
                     [call-stack : (U False JSHash)]
                     [extra-info : (U False JSHash)]
                     [lines-left : (U False (Listof String))]))
(define no-data (SectionData #f #f #f #f)) 


;; Convenience function that creates a SectionParser that checks if the
;; first line of the input matches pattern. If so, returns a SectionData
;; with the type set to new-type.
(: match-type (PRegexp String -> SectionParser))
(define (match-type pattern new-type)
  (lambda ([lines : (Listof String)] [error-type : String] [call-stacks : JSList] [extra-info : JSHash])
    (if (and (cons? lines) (regexp-match pattern (first lines)))
        (SectionData new-type #f #f (rest lines))
        no-data)))

;; Another convenience function for creating SectionParsers. If the first line
;; matches pattern, then process-match-result function will be called with
;; the ASAN output (lines) and the match result.
(: match-and-process (PRegexp (String (Listof String) (Listof String) -> SectionData) -> SectionParser))
(define (match-and-process pattern process-match-result)
  (lambda ([lines : (Listof String)] [error-type : String] [call-stacks : JSList] [extra-info : JSHash])
    (define match-result (regexp-match pattern (first lines)))
    (if match-result (process-match-result error-type lines match-result) no-data)))

;; Tries to parse a memory-leak section. In ASAN output, a memory leak section
;; starts with the regexp pattern below, followed by a frame list.
(define memory-leak-parser : SectionParser
  (match-and-process
   #px"^[[:alpha:]]+ leak of (\\d+) byte\\(s\\) in (\\d+) object\\(s\\) allocated from:"
   (lambda ([error-type : String] [lines : (Listof String)] [match-result : (Listof String)])
     (define extra-info (jsexpr `((leak_size_in_bytes ,(second match-result))
                                  (leak_objects_count ,(third match-result)))))
     (define-values (framelist lines-left) (try-parse-stack-frame lines))
     (SectionData #f ; error type
                  (jsexpr `((framelist ,framelist) (misc ,extra-info)))
                  #f ; global extra info
                  lines-left))))

;; If the student does stack overflow, ASAN prints out a section containing the
;; regexp below followed by a frame list
(define stack-overflow-parser : SectionParser
  (match-and-process
   #px"^[[:alpha:]]+ of size (\\d+) at (0x[[:xdigit:]]+) thread T"
   (lambda ([error-type : String] [lines : (Listof String)] [match-result : (Listof String)])
     (define extra-info (jsexpr `((description_of_this_framelist "Location of bad memory access")
                                  (size_of_memory_accessed_in_bytes ,(second match-result))
                                  (address_of_memory_accessed ,(third match-result)))))
     (define-values (framelist lines-left) (try-parse-stack-frame lines))
     (SectionData #f ; error type
                  (jsexpr `((framelist ,framelist) (misc ,extra-info)))
                  #f ; global extra info
                  lines-left))))

;; When a stack overflow occurs, ASAN also seems to print out some information
;; about the function itself
(define function-info : SectionParser
  (match-and-process
   #px"^Address (0x[[:xdigit:]]+) is located in stack of thread T(\\d+) at offset (\\d+) in frame"
   (lambda ([error-type : String] [lines : (Listof String)] [match-result : (Listof String)])
     (define extra-info (jsexpr `((address_of_memory_accessed ,(second match-result))
                                  (offset_in_frame_of_memory_accessed ,(fourth match-result)))))
     (define-values (framelist lines-left) (try-parse-stack-frame lines))
     (SectionData #f ; error type
                  (jsexpr `((framelist ,framelist) (misc ,extra-info)))
                  #f ; global extra info
                  lines-left))))


;; ASAN prints out the variable where the over/underflow occurred. This error message appears
;; for stack buffer over/underflows, as well as stack use after return/scope.
(define array-parser : SectionParser
  (match-and-process
   #px"\\[(\\d+), (\\d+)\\) '([[:alnum:]_]*)' <== Memory access at offset (\\d+)"
   (lambda ([error-type : String] [lines : (Listof String)] [match-result : (Listof String)])
     (define array-lower-bound (string->number (second match-result)))
     (define array-upper-bound (string->number (third match-result)))
     (define access-location (string->number (fifth match-result)))
     (define extra-info (jsexpr (cons (if (< access-location array-upper-bound)
                                          (list 'underflow_distance_in_bytes_from_start_of_array (number->string (- array-lower-bound access-location)))
                                          (list 'overflow_distance_in_bytes_from_end_of_array (number->string (- access-location array-upper-bound))))
                                      `((array_size_in_bytes ,(number->string (- array-upper-bound array-lower-bound)))
                                        (array_variable_name ,(fourth match-result))))))
     (SectionData (if (and (equal? error-type "stack-buffer-overflow") (< access-location array-upper-bound))
                      "stack-buffer-underflow" #f)
                  #f ; frame list
                  (if (not (equal? (fourth match-result) "")) extra-info #f)
                  #f)))) ; lines left

;; For bad heap accesses, ASAN sometimes print info about where the heap access occurred
(define heap-address-details : SectionParser
  (match-and-process
   #px"(0x[[:xdigit:]]+) (is located (\\d+) bytes (to the left|to the right|inside) of (\\d+)-byte region \\[(0x[[:xdigit:]]+),(0x[[:xdigit:]]+)\\))"
   (lambda ([error-type : String] [lines : (Listof String)] [match-result : (Listof String)])
     (define extra-info (jsexpr (list (list (string->symbol (string-append "details_of_address_" (second match-result)))
                                            (third match-result)))))
     (SectionData #f #f extra-info #f))))

;; Similarly, ASAN prints details of bad memory accesses in the global region
(define global-address-details : SectionParser
  (match-and-process
   #px"(0x[[:xdigit:]]+) (is located (\\d+) bytes (to the left|to the right|inside) of global variable '([[:alnum:]_]+)') defined in '([^:']+):(\\d+):(\\d+)' \\(0x[[:xdigit:]]+\\) of size (\\d+)"
   (lambda ([error-type : String] [lines : (Listof String)] [match-result : (Listof String)])
     (define address (second match-result))
     (define variable-name (sixth match-result))
     (define file-path-absolute (seventh match-result))
     (define file-name (last (string-split file-path-absolute "/")))
     (define line-nbr (eighth match-result))
     (define col-nbr (ninth match-result))
     (define variable-size (tenth match-result))
     (define extra-info (jsexpr (list (list (string->symbol (string-append "details_of_address_" address))
                                            (third match-result))
                                      (list (string->symbol (string-append variable-name "_is_defined_in_file")) file-name)
                                      (list (string->symbol (string-append variable-name "_definition_line_number")) line-nbr)
                                      (list (string->symbol (string-append variable-name "_definition_column_number")) col-nbr)
                                      (list (string->symbol (string-append variable-name "_size_in_bytes")) variable-size))))
     (SectionData #f #f extra-info #f))))


;; When ASAN detects memory that is free'd multiple times, it prints 3 stack frame sections:
;; #1: Where the second free occurred
;; #2: Where the first free occurred
;; #3: Where the allocation took place
(define double-free : SectionParser
  (match-and-process
   #px"^=+\\d+=+ERROR: AddressSanitizer: attempting double-free on (0x[[:xdigit:]]+) in thread"
   (lambda ([error-type : String] [lines : (Listof String)] [match-result : (Listof String)])
     (define extra-info (jsexpr `((description_of_this_framelist "Location of second free")
                                  (double_free_at_address ,(second match-result)))))
     (define-values (framelist lines-left) (try-parse-stack-frame lines))
     (SectionData "double-free" ; error type
                  (jsexpr `((framelist ,framelist) (misc ,extra-info)))
                  #f ; global extra info
                  lines-left))))

(define double-free-first-free : SectionParser
  (match-and-process
   #px"freed by thread T\\d+ here:"
   (lambda ([error-type : String] [lines : (Listof String)] [match-result : (Listof String)])
     (define extra-info (jsexpr '((description_of_this_framelist "Location of a free"))))
     (define-values (framelist lines-left) (try-parse-stack-frame lines))
     (SectionData #f ; error type
                  (jsexpr `((framelist ,framelist) (misc ,extra-info)))
                  #f ; global extra info
                  lines-left))))

(define allocation-details : SectionParser
  (match-and-process
   #px"allocated by thread T\\d+ here:"
   (lambda ([error-type : String] [lines : (Listof String)] [match-result : (Listof String)])
     (define extra-info (jsexpr '((description_of_this_framelist "Location of memory allocation"))))
     (define-values (framelist lines-left) (try-parse-stack-frame lines))
     (SectionData #f ; error type
                  (jsexpr `((framelist ,framelist) (misc ,extra-info)))
                  #f ; global extra info
                  lines-left))))

;; ASAN prints this when the student frees something that wasn't malloc'ed
(define free-non-malloc : SectionParser
  (match-and-process
   #px"^=+\\d+=+ERROR: AddressSanitizer: attempting free on address which was not malloc\\(\\)-ed: (0x[[:xdigit:]]+) in thread T"
   (lambda ([error-type : String] [lines : (Listof String)] [match-result : (Listof String)])
     (define extra-info (jsexpr `((tried_to_free_this_address ,(second match-result)))))
     (define-values (framelist lines-left) (try-parse-stack-frame lines))
     (SectionData "free-non-malloced-address" ; error type
                  (jsexpr `((framelist ,framelist) (misc ,extra-info)))
                  #f ; global extra info
                  lines-left))))

;; Try to parse a frame
(define frame-parse : SectionParser
  (match-and-process
   #px"^\\{\"frame\": "
   (lambda ([error-type : String] [lines : (Listof String)] [match-result : (Listof String)])
     (define-values (framelist lines-left) (try-parse-stack-frame lines))
     (SectionData #f ; error type
                  (jsexpr `((framelist ,framelist) (misc ,(hash))))
                  #f ; global extra info
                  lines-left))))

;; A big list of functions to try and parse the input lines.
(define all-parsers : ParserList
  (list (match-type #px"^=+\\d+=+ERROR: AddressSanitizer: SEGV on unknown address 0x0+ " "segmentation-fault-on-null-address")
        (match-type #px"^=+\\d+=+ERROR: AddressSanitizer: SEGV" "segmentation-fault")
        (match-type #px"^=+\\d+=+ERROR: AddressSanitizer: stack-buffer-overflow" "stack-buffer-overflow")
        (match-type #px"^=+\\d+=+ERROR: AddressSanitizer: global-buffer-overflow" "global-buffer-overflow")
        (match-type #px"^=+\\d+=+ERROR: AddressSanitizer: heap-buffer-overflow" "heap-buffer-overflow")
        (match-type #px"^=+\\d+=+ERROR: AddressSanitizer: heap-use-after-free"  "heap-use-after-free")
        (match-type #px"^=+\\d+=+ERROR: AddressSanitizer: stack-use-after-return"  "stack-use-after-return")
        (match-type #px"^=+\\d+=+ERROR: AddressSanitizer: stack-use-after-scope"  "stack-use-after-scope")
        (match-type #px"^=+\\d+=+ERROR: LeakSanitizer: detected memory leaks" "memory-leak")
        memory-leak-parser
        stack-overflow-parser function-info array-parser
        heap-address-details global-address-details
        double-free double-free-first-free allocation-details
        free-non-malloc
        frame-parse
        ))

(: try-parsers ((Listof String) String JSList JSHash  ParserList -> SectionData))
(define (try-parsers lines error-type call-stacks extra-info section-parsers)
  (cond [(or (empty? lines) (empty? section-parsers)) no-data]
        [else (define result ((first section-parsers) lines error-type call-stacks extra-info))
              (if (equal? result no-data)
                  (try-parsers lines error-type call-stacks extra-info (rest section-parsers))
                  result)]))


;; Purpose: Consumes a string raw-asan-output which is the raw output from ASAN. Will parse raw-asan-output, stripping
;; out "scary" stuff that students don't care about, and putting important information in a JSON format for the front-end.
;; This function is mostly just a wrapper that converts the input into a list of strings, passes it to asan-parser,
;; which does most of the work.
(: asan->json (Bytes -> Bytes))
(define (asan->json raw-asan-output)
  (define raw-asan-output-str (bytes->string/utf-8 raw-asan-output))
  (string->bytes/utf-8
   (jsexpr->string
    (jsexpr-add (asan-parser (regexp-split #px"\n\\s*" raw-asan-output-str)
                             "unknown" empty (hash))
                'raw_message raw-asan-output-str))))


(: asan-parser ((Listof String) String JSList JSHash -> JSHash))
(define (asan-parser lines error-type call-stacks extra-info)
  (cond [(empty? lines)
         (hash 'error_type error-type
               'call_stacks call-stacks
               'misc extra-info)]
        [else ;; Use the functions in all-parsers to try and parse the first several lines
         (define section-result : SectionData (try-parsers lines error-type call-stacks extra-info all-parsers))
         (define new-lines-left (SectionData-lines-left section-result))
         (define new-error-type (SectionData-error-type section-result))
         (define new-call-stack (SectionData-call-stack section-result))
         (define new-extra-info (SectionData-extra-info section-result))
         (asan-parser (if new-lines-left new-lines-left (rest lines))
                      (if new-error-type new-error-type error-type)
                      (if new-call-stack (cons new-call-stack call-stacks) call-stacks)
                      (if new-extra-info (hash-union new-extra-info extra-info) extra-info))]))


(: try-parse-stack-line (String -> (U JSHash False)))
(define (try-parse-stack-line aline)
  (with-handlers ([exn:fail? (lambda (exn) #f)])
    ;; string->jsexpr may return eof (and possible other values?)
    (define result (string->jsexpr aline))
    (if (and (jsexpr? result) (hash? result))
        result #f)))

;; Tries to extract a stack frame
;; returns: List of parsed stack frames
;;          Unparsed lines from lines
(: try-parse-stack-frame (->* ((Listof String)) ((Listof JSHash)) (values (Listof JSHash) (Listof String))))
(define (try-parse-stack-frame lines [frames-list empty])
  (cond [(empty? lines) (values frames-list lines)]
        [else (define result (try-parse-stack-line (first lines)))
              (cond [(and (not result) (cons? frames-list)) (values frames-list lines)]
                    [(not result) (try-parse-stack-frame (rest lines) frames-list)]
                    [(let* ((filename (jsexpr-ref result 'file))
                            (seashell-path-string (read-config-path 'seashell))
                            (seashell-folder (if (string? seashell-path-string)
                                                 seashell-path-string
                                                 (path->string seashell-path-string))))
                       (and (string? filename) (string-prefix? filename seashell-folder)))
                     (try-parse-stack-frame (rest lines) (cons result frames-list))]
                    [else (try-parse-stack-frame (rest lines) frames-list)])]))
