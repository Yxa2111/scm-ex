(define (fib n)
  (define (fib-iter a b q p i)
    (cond ((= i 0) b)
	  ((even? i)
	   (fib-iter a b
		     (+ (square q) (* 2 p q))
		     (+ (square p) (square q))
		     (/ i 2)))
	  (else
	    (fib-iter (+ (* b q) (* a (+ p q)))
		      (+ (* b p) (* a q))
		      q p (- i 1)))))
  (fib-iter 1 0 1 0 n))

(fib 5)
(fib 6)
(fib 7)
(fib 8)
(fib 100000)
(fib 99999)
(fib 99998)