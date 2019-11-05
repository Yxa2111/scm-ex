(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (print-point p)
  (display "(")
  (display (y-point p))
  (display ",")
  (display (x-point p))
  (display ")"))

(define (print-segment s)
  (print-point (start-segment s))
  (display "->")
  (print-point (end-segment s)))

(define (midpoint-segment s)
  (make-point
    (/ (+ (x-point (start-segment s))
	  (x-point (end-segment s)))
       2)
    (/ (+ (y-point (start-segment s))
	  (y-point (end-segment s)))
       2)))

(define s1 (make-segment (make-point 1 1) (make-point 2 3)))
(define s2 (make-segment (make-point 1 0) (make-point 4 5)))
(newline)
(print-segment s1)
(newline)
(print-segment s2)
(print-point (midpoint-segment s1))
