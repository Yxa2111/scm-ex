;fast-expt

(define (fast-expt a n)
  (define (iter a n ans)
     (cond ((= n 0) ans)
	   ((even? n) (iter (square a) (/ n 2) ans))
	   (else (iter (square a) (/ (- n 1) 2) (* ans a)))))
      ;or  (else (iter a (- n 1) (* ans a)))
  (iter a n 1))

(fast-expt 2 31)
(fast-expt 2 10)
(fast-expt 3 3)
(fast-expt 2 64)
(fast-expt 2 30)
