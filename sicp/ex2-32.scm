(define (subsets s)
  (if (null? s)
      (list s)
      (let ((rest (subsets (cdr s)))
	    (now (car s)))
	(append rest (map
		       (lambda (lst) (cons now lst))
		       rest)))))

(subsets (list 1 2 3))
