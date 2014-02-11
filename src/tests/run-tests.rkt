#lang racket

(require (prefix-in contract: racket/contract))

;; Number of seconds to wait after starting all of the subprocesses
(define test-timeout 10)

;; Directory where tests are located
(define test-dir "./tests")

;; File types to run
(define extensions '(#"exe" #"rkt"))

;; gets the racket executable from path variable
(define rktPath (find-executable-path "racket"))

;; removes existing log file if it exists, and open it up as a file port
(void (system "rm -f test-log.txt"))
(define out-file (open-output-file "test-log.txt"))

;; (file-filter f)
;; returns #t if the given path is a file with a type that the
;; tets harness should run.
(define/contract (file-filter f)
   (contract:-> path-string? boolean?)
   (define ext (filename-extension f))
   (and ext (not (false? (member ext extensions)))))

;; (begin-subprocess path)
;; starts a subprocess for the file located at path, produces a list
;; of the form (list path subprocess stdout-port stdin-port)
;;
;; for racket files, it uses racket to run them
;; for all other files it just tries to execute the file itself
;;
;; directs all stderr output to the test log file.
(define/contract (begin-subprocess path)
   (contract:-> path-string? list?)
   (define ext (filename-extension path))
   (define command (path->string path))
   (cons path
      (cond
         [(bytes=? ext #"rkt")
            (let-values ([(sub out in err)
               (subprocess #f #f out-file rktPath command)])
               (list sub out in))]
         [else (let-values ([(sub out in err)
            (subprocess #f #f out-file command)])
            (list sub out in))])))

;; (finish-subprocess proc)
;; consumes a list of the form produced by begin-subprocess,
;; terminates the process if it is still running,
;; if the process exited with code 0, parses output as follows:
;;    first line is an integer denoting the number of tests passed
;;    second line is an integer denoting total number of tests
;;    (it displays a message if these are not equal)
;; if the process exited with any other code, displays a message
(define/contract (finish-subprocess proc)
   (contract:-> list? void?)
   (define passed (read (third proc)))
   (define total (read (third proc)))
   (define status (subprocess-status (second proc)))
   (cond
      [(not (third proc)) (void)]
      [(and (symbol? status) (symbol=? status 'running))
         (printf "Test located at ~a timed out.\n" (first proc))
         (subprocess-kill (third proc))]
      [(= status 0)
         (when (not (= passed total))
            (printf "~a tests failed in ~a\n" (- total passed)
               (first proc)))]
      [else (printf "~a exited with status: ~a\n" (first proc) status)])
   (close-input-port (third proc))
   (close-output-port (fourth proc)))

;; get a list of all test files
(define files (find-files file-filter test-dir))
;; start all the subprocesses
(define running (map begin-subprocess files))
;; give them some time
(void (system (format "sleep ~a" test-timeout)))
;; check the results of the subprocesses
(void (map finish-subprocess running))
;; close the test log file
(close-output-port out-file)

