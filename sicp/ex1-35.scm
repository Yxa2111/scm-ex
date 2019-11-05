(define tolerance 0.00001)
(define (fixed-point f guess)
  (define (enough? v1 v2)
    (<= (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (enough? next guess)
	  guess
	  (try next))))
  (try guess))

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 0.0)

