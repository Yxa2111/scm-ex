(define (reverse-lst lst)
  (define (iter lst rev-lst)
    (cond ((null? lst) rev-lst)
	  (else (iter (cdr lst) (cons (car lst) rev-lst)))))
  (iter lst (list)))

(reverse-lst (list 1 2 3 4 5 6 7))
(reverse-lst (list))
(reverse-lst (list 1))
