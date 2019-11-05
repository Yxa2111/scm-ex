(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (inc x)
  (+ x 1))

((compose square inc) 6)
;ex1-42

(define (repeated f n)
  (define (iter f-tot i)
    (if (= i 0)
	f-tot
	(iter (compose f f-tot) (- i 1))))
  (iter f (- n 1)))

((repeated square 2) 5)
