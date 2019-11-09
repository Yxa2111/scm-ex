(define (make-mobile left right)
  (list left right))

(define (make-branch len structure)
  (list len structure))

(define (left-branch m)
  (car m))

(define (right-branch m)
  (cadr m))

(define (branch-length br)
  (car br))

(define (branch-struct br)
  (cadr br))

(define (branch? br)
  (and (pair? br) (not (pair? (car br)))))

(define (branch-weight br)
  (* (branch-length br) (total-weight (branch-struct br))))

(define (total-weight m)
  (if (not (pair? m))
      m
      (let ((lbr (left-branch m)) (rbr (right-branch m)))
	(+ (branch-weight lbr)
	   (branch-weight rbr)))))

(define (mobile-bal? m)
  (define (print w1 w2)
    (newline)
    (display "left: ")
    (display w1)
    (display " right: ")
    (display w2))
  (let ((lbr-w (branch-weight (left-branch m))) (rbr-w (branch-weight (right-branch m))))
    ((if (= lbr-w rbr-w)
	((newline)
	 (display " euqal!"))
	((newline)
	 (display " not equal!")))
     (print lbr-w rbr-w)
     #t)))

(define mobi
  (make-mobile
    (make-branch 4
		 (make-mobile (make-branch 2 1)
			      (make-branch 1 1)))
    (make-branch 3
		 (make-mobile (make-branch 1 2)
			      (make-branch 2 2)))))

(total-weight mobi)
(mobile-bal? mobi)
