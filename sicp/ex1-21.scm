(define (find-divisor n a)
  (cond ((> (square a) n) n)
	((= (modulo n a) 0) a)
	(else (find-divisor n (+ a 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (and (> n 1) (= (smallest-divisor n) n)))
(prime? 1)
(prime? 74)
(prime? 51)

(prime? 1999)
(smallest-divisor 51)
