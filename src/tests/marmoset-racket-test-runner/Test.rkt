#lang racket

(require file/md5)

(define your-student-number     (begin (read) (read) (read)))
(define my-c-magic-number       (begin (read) (read) (read) (read)))
(define my-racket-magic-number  (begin (read) (read) (read) (read)))

(cond
  [(and
     (equal? (number->string my-racket-magic-number) (number->string (apply + (bytes->list (md5 (number->string your-student-number))))))
     (equal? my-c-magic-number (+ 136 (remainder your-student-number 136))))
     (printf "Seashell Test Passed\n")]
  [else (printf "Seashell Test Failed\n")])

