;ex1
(define (int2char num)
  (if (and (> num 32) (< num 127))
    (integer->char num) #f))
(int2char 98)
(int2char (+ 97 25))
(> 1 3)
;ex2
(define (>=0 num) (if (>= num 0) #t #f))
(define (<0 num) (not (>=0 num)))
(define (f1 a b c) (if (and (>=0 a) (>=0 b) (>=0 c)) (* a b c) #f))
(define (f2 a b c) (if (and (<0 a) (<0 b) (<0 c)) (* a b c) #f))

(f1 3 2 4)
(f1 3 -2.1 5)
(f2 -1 -2 -7)
(f2 -1 2 6)

;ex3
(define (f3 score)
  (cond
    ((>= score 80) 'A)
    ((>= score 60) 'B)
    ((>= score 40) 'C)
    (else 'D)))

(f3 45)
(f3 79)
(f3 99)
(f3 66)

(eqv? 1.0 1)
(eqv? 1 1)
(eqv? '(1 2 3) '(1 2 3))
(eqv? "str" "str")
(equal? "str" "str")

