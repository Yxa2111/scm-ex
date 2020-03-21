(define (f-iter1 n) 
  (if (< n 3)
      n
      (+ (f-iter1 (- n 1))
	 (* 2 (f-iter1 (- n 2)))
	 (* 3 (f-iter1 (- n 3))))))

;(f-iter1 30)

(define (f-iter2 a b c n)
  (if (< n 3)
      a
      (f-iter2 (+ a (* 2 b) (* 3 c)) a b (- n 1))))

(define (f-2 n)
  (if (< n 3)
      n
      (f-iter2 2 1 0 n)))

(f-2 1000)
