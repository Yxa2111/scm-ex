(define (good-enough? guess x)
  ( < (abs (- (* guess guess guess) x)) 0.001))

(define (improve y x)
  (/ (+ (* 2 y) ( / x (* y y))) 3))

(define (cbrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (cbrt-iter (improve guess x) x)))

(define (cbrt x)
  (cbrt-iter 1.0 x))

(cbrt 27)
(cbrt 128)
(cbrt 216)
