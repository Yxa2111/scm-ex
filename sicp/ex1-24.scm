(define (fast-expt a b m)
  (define (mult-mod a b m)
    (if (> m 0)
	(modulo (* a b) m)
	(* a b)))
  (let loop((x a) (y b) (ans 1))
    (cond ((= y 0) ans)
	  ((even? y) (loop (mult-mod x x m) (/ y 2) ans))
	  (else (loop x (- y 1) (mult-mod ans x m))))))

(define (fermat-test n)
  (let ((a (+ 1 (random (- n 1)))))
    (= (fast-expt a n n) a)))

(define (fast-prime? n)
  (let loop((times 10))
    (cond ((= times 0) #t)
	((fermat-test n) (loop (- times 1)))
	(else #f))))

(cd "/home/yxa/scm-ex/sicp")
(load "ex1-22.scm")


(search-for-primes 100000000000000000000 999999999999999999999999999999 fast-prime?)
(search-for-primes 1000000000000000000000 999999999999999999999999999999 fast-prime?)
(search-for-primes 10000000000000000000000 999999999999999999999999999999 fast-prime?)
