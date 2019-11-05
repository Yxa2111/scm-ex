(define dx 0.00001)
(cd "/home/yxaa/scm-ex/sicp")
(load "ex1-35.scm")
(define (deriv f)
  (lambda (x)
    (/
      (- (f (+ x dx))
	 (f x))
      dx)))

(define (newton-transform f)
  (lambda (x)
    (- x
       (/
	 (f x)
	 ((deriv f) x)))))

(define (newton-method f guess)
  (fixed-point (newton-transform f) guess))

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) (* a x x) (* b x) c)))

(define ans (newton-method (cubic 4 0 1) 1))

((cubic 4 0 1) ans)
