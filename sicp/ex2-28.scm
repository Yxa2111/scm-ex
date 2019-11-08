(define x (list (list (list 5 (list 6)) (list 2 7)) (list (list 9 10) 4)))

(define (fringe tree)
  (cond ((pair? tree)
	 (append (fringe (car tree)) (fringe (cdr tree))))
	((null? tree) tree)
	(else (list tree))))

(fringe x)
