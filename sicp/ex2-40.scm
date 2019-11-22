(define (accu op initial seq)
  (if (null? seq)
      initial
      (op (car seq) (accu op initial (cdr seq)))))

(define (filter condition seq)
  (cond ((null? seq) seq)
	((condition (car seq))
	 (cons (car seq) (filter condition (cdr seq))))
	(else (filter condition (cdr seq)))))

(define (make-seq from to)
  (if (> from to)
      '()
      (cons from (make-seq (+ from 1) to))))

(define (unique-pairs n)
  (accu (lambda (num lst) (append (list (cons n num)) lst)) (list) (make-seq 1 (- n 1))))

(unique-pairs 5)

(filter (lambda (x) (= (modulo x 2) 0)) (make-seq 1 10))

(define (prime? n)
  (define (iter i)
    (cond ((> (* i i) n) #t)
	  ((= (modulo n i) 0) #f)
	  (else (iter (+ i 1)))))
  (cond ((< n 1) #f)
	(else (iter 2))))

(define (prime-sum-pairs n)
  (define (make-pairs n)
    (accu (lambda (x y) (append (unique-pairs x) y))
	  (list)
	  (make-seq 1 n)))
    (filter (lambda (x) (prime? (+ (car x) (cdr x))))
	  (make-pairs n)))
;O(n^2 * sqrt(n))
(prime-sum-pairs 100)
