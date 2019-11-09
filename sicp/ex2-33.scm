(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
	  (accumulate op initial (cdr seq)))))

(define (Map p seq)
  (accumulate (lambda (x y) (cons (p x) y)) '() seq))

(define (Append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (Length seq)
  (accumulate (lambda (x y) (+ y 1)) 0 seq))

(define lst (list 1 2 3 4 5 6))

(Map square lst)
(Append lst (list 7 8 9 10 11))
(Length (Append lst (list 7 8 9 10)))
