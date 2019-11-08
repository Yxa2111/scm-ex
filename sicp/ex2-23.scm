(define (foreach func lst)
  (if (null? lst)
      #t
      (let ((item (car lst))
	    (rest (cdr lst)))
	(and (func item)
             (foreach func rest)))))

(foreach (lambda (x) (newline) (display x)) (list 1 2 3 4 5 6))
