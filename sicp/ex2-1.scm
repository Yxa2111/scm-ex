(define (make-rat n d)
  (let ((n1 (abs n)) (d1 (abs d)) (g (gcd n d)))
    (if (< (* n d) 0)
	(cons (* -1 (/ n1 g)) (/ d1 g))
	(cons (/ n1 g) (/ d1 g)))))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	       (* (denom x) (denom y))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(print-rat (add-rat (make-rat -1 2) (make-rat -2 -8)))
