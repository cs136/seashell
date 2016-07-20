#lang racket

(require rackunit
         seashell/seashell-config
         seashell/crypto)

(define/provide-test-suite crypto-suite
  (test-suite "Crypto Tests"
    (test-case "Encrypt and decrypt"
      (define enc (let-values
        ([(a b c) (seashell-encrypt #"Kaleb is cool!!!" #"bababooey" #"hey")])
        (list a b c)))
      (check bytes=? (seashell-decrypt #"Kaleb is cool!!!"
        (first enc) (third enc) (second enc) #"hey")
        #"bababooey"))
    
    (test-case "Decrypting Nonsense"
      (check-exn exn:fail?
        (thunk (seashell-decrypt
          #"Kaleb is cool!!!"
          #"012345678901"
          #"0123456789012345"
          #"some garbage"
          #"garbage"))))

    (test-case "seashell-crypt-key->client"
      (check-equal? (seashell-crypt-key->client #"0123456789012345")
        '(808530483 875902519 943271985 842216501)))))
