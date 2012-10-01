#!/scratch/m4burns/racket/bin/racket
#lang racket
(require racket/date)
(require net/smtp)

;; config

(define listen-port 99)
(define listen-address "129.97.134.70")
(define keys-file "keys")
(define scores-file "scores")
(define pre-test-hooks
	'( "iptables -A OUTPUT -p tcp -m owner --uid-owner opencl -j DROP" ))

(define drop-permissions-binary "/opt/cltest/root/drop")
(define test-binary "test test")
(define test-directory "/opt/cltest/test")

;; tcp socket

(define listener (tcp-listen listen-port 3 #t listen-address))

;; key/email deserialization and logic

(define keys (map (lambda(c) (cons (car c) (number->string (cdr c)))) (with-input-from-file keys-file read)))

(define (validate-key e k)
	(let ((ka (assoc e keys)))
		(and ka (equal? k (cdr ka)))))

;; scores serialization and logic

(define scores-semaphore (make-semaphore 1))

(define (read-scores)
	(make-hash (with-input-from-file scores-file read)))

(define (write-scores sc)
	(call-with-semaphore scores-semaphore
		(thunk (with-output-to-file scores-file (thunk (write (hash->list sc))) #:exists 'truncate) sc)))

(define (score-lt l r)
	(< (second l) (second r)))

(define (update-score name question language score)
	(call-with-semaphore scores-semaphore
		(thunk
			(unless (and (hash-has-key? scores (list name question))
									 (score-lt (list language score)
														 (hash-ref scores (list name question))))
						(hash-set! scores (list name question) (list language score)))))
	(write-scores scores))

(define (get-scores)
	(call-with-semaphore scores-semaphore
		(thunk
			(sort (hash-map scores (lambda(k v) (cons k v)))
						(lambda(x y) (score-lt (cdr x) (cdr y)))))))

(define scores (read-scores))

;; judge dispatch

(define (dispatch-judge fn em key q l)
	(thread (lambda()
		(with-handlers ([exn? (lambda(e) (display e) (newline))])
			(judge fn em key q l)))))

;; judge

(define (send-result em msg)
	(printf "emailed to ~a: ~a~n" em msg)
  (smtp-send-message
		"caffeine.csclub.uwaterloo.ca"
		"opencl-judge@csclub.uwaterloo.ca"
		(list em)
		"Subject: OpenCL Judge Message\n\n"
		(list msg)))

(define safe-q '(("q1" . "q1") ("q2" . "q2") ("q3" . "q3") ("q4" . "q4")))
(define safe-l '(("c" . "c") ("cc" . "c++")))

(define (judge fn em key q l)
	(for-each (lambda(v) (system v)) pre-test-hooks)
	(let* ((sq (assoc q safe-q))
				 (sl (assoc l safe-l))
				 (sfn (build-path "/opt/cljudge/submissions" (file-name-from-path fn)))
				 (startts (current-date))
				 (testdir (make-temporary-file "sub~a" 'directory test-directory))
				 (testzip (make-temporary-file "z~a.zip" sfn testdir)))
		(if (and sq sl sfn)
			(begin
				(system (string-append "chown -R opencl:opencl " (path->string testdir)))
				(system (string-append "chmod -R o-rwx " (path->string testdir)))
				(let*
					( (vl
						 (process (string-append drop-permissions-binary " " test-binary " " (cdr sq) " " (path->string testzip) " " (cdr sl))))
						(in (first vl))
						(out (second vl))
						(pid (third vl))
						(err (fourth vl))
						(s (fifth vl)) )
					(s 'wait)
					(let ((rv (s 'exit-code)))
						(if (= 0 rv)
							(let ((out-contents (port->string in)))
								(update-score em (cdr sq) (cdr sl) (string->number out-contents))
								(send-result em
									(with-output-to-string (thunk
										(printf "Your submission at ~a successfully completed question ~a in ~a seconds. This has been recorded on the score board."
														(date->string startts #t) (cdr sq) (string->number out-contents))))))
							(send-result em
									(with-output-to-string (thunk
										(printf "Your submission at ~a did not return a correct result for question ~a."
														(date->string startts #t) (cdr sq)))))))))
			(send-result em
				"Your submission could not be tested because the judge received invalid input. Please contact syscom@csclub.uwaterloo.ca for help."))))

;; scores table display

(define (display-scores-table out)
	(let ((st (get-scores)))
		(fprintf out "~a~n" (add1 (length st)))
		(fprintf out
			"<b>Submitter</b>:<b>Question</b>:<b>Language</b>:<b>Score (seconds)</b>~n")
		(for-each (lambda(v)
			(fprintf out "~a:~a:~a:~a~n"
				(caar v)
				(cadar v)
				(cadr v)
				(caddr v)))
			st)))

;; main loop

(define (listen-forever)
	(let-values ( ((in out) (tcp-accept listener)) )
		(match (call-with-input-string (read-line in) read)
			[`(judge ,fn ,email ,key ,question ,language)
				(fprintf out
					(if (validate-key email key)
						(begin
							(dispatch-judge fn email key question language)
							"ok~n")
						"no~n"))]
			[`(scores)
			  (display-scores-table out)]
			[else
				(fprintf out "go away~n")])
		(close-output-port out)
		(close-input-port in))
	(listen-forever))

(listen-forever)

