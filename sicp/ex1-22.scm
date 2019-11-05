(cd "/home/yxa/scm-ex/sicp")
(load "ti-test.scm")
(load "ex1-21.scm")

(define (search-for-primes lower upper p)
  (cond ((>= lower upper) #f)
      ((even? lower) (search-for-primes (+ lower 1) upper p))
    	((ti-test lower p) lower)
      	(else (search-for-primes (+ lower 2) upper p))))

(search-for-primes 1000000000 99999999999999999999 prime?)
(search-for-primes 10000000000 99999999999999999999 prime?)
(search-for-primes 100000000000 9999999999999999999 prime?)

(define (sm-div n a)
  (cond ((> (square a) n) n)
	((= 0 (modulo n a)) a)
	(else (sm-div n (next-div a)))))

(define (next-div a)
  (if (= a 2)
      3
      (+ a 2)))

(define (2-prime? n)
  (and (> n 1) (= (sm-div n 2) n)))


(search-for-primes 1000000000 99999999999999999999 2-prime?)
(search-for-primes 10000000000 99999999999999999999 2-prime?)
(search-for-primes 100000000000 9999999999999999999 2-prime?)

