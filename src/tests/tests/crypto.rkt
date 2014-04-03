#lang racket

(require seashell/seashell-config
         seashell/crypto)

(define total-tests 3)
(define passed 0)

(config-set! 'test-mode #t)

;; Test 1: Encrypt and decrypt some byte string

(define enc (let-values
	([(a b c) (seashell-encrypt #"Kaleb is cool!!!" #"bababooey" #"hey")])
	(list a b c)))

(if (bytes=? (seashell-decrypt #"Kaleb is cool!!!"
	(first enc)
	(third enc)
	(second enc)
	#"hey")
	#"bababooey")
   (set! passed (add1 passed))
   (display "Could not encrypt and decrypt a byte string.\n" (current-error-port)))

;; Test 2 - Decrypting nonsense

(with-handlers ([exn? (lambda (x) (set! passed (add1 passed)))])
	(seashell-decrypt #"Kaleb is cool!!!"
		#"012345678901"
		#"0123456789012345"
		#"some garbage"
		#"garbage")
	(display "Decrypting garbage worked.\n" (current-error-port)))

(if (equal? (seashell-crypt-key->client #"0123456789012345")
   '(808530483 875902519 943271985 842216501))
   (set! passed (add1 passed))
   (display "seashell-crypt-key->client failed.\n" (current-error-port)))

(printf "~a\n~a\n" passed total-tests)
