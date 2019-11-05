(define (cont-frac n d k)
  (let iter((ans (/
		   (n k)
		   (d k)))
	    (i (- k 1)))
    (let ((next-n (n i))
	  (next-d (d i)))
      (if (< i 1)
	ans
	(iter 
	  (/ next-n
	     (+ next-d ans))
	  (- i 1))))))

(cont-frac (lambda (i) 1.0)
	   (lambda (i) 1.0)
	   100)
