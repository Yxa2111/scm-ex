(define (new-if condition p1 p2)
        (cond (condition p1)
	      (else p2)))

(define (good-enough? guess x)
  (< (abs (- (* guess guess) x)) 0.0000001))

(define (improve guess x)
  (/ (+ guess (/ x guess)) 2.0))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x)
		     x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)

(sqrt 2)
;it's not correct because it eval three args at first. Finally is cause infinte loop.
