#lang planet neil/sicp

(define (fast-expt base exponment)
  (let loop((a base) (b exponment) (res 1))
    (cond ((= b 0) res)
	  ((= (modulo b 2) 0)
	   (loop (* a a) (/ b 2) res))
	  (else (loop (* a a) (/ (- b 1) 2) (* res a))))))

(define (=number? x num)
  (and (number? x) (= x num)))
(define (var? x) (symbol? x))
(define (same-var? v1 v2) (eq? v1 v2))

(define (make-sum a1 a2)
  (cond
    ((=number? a1 0) a2)
    ((=number? a2 0) a1)
    ((and (number? a1) (number? a2))
	 (+ a1 a2))
    ((and (var? a1) (var? a2) (same-var? a1 a2))
     (make-product 2 a1))
    (else (list '+ a1 a2))))

(define (make-product a1 a2)
  (cond
    ((or (=number? a1 0) (=number? a2 0)) 0)
    ((=number? a1 1) a2)
    ((=number? a2 1) a1)
    ((and (number? a1) (number? a2)) (* a1 a2))
    ((and (var? a1) (var? a2) (same-var? a1 a2)) (make-expn a1 2))
    ((and (expn? a1) (expn? a2) (eq? (base a1) (base a2)))
     (make-expn a1 (make-sum (exponment a1) (exponment a2))))
    (else (list '* a1 a2))))

(define (make-expn base exponment)
  (cond ((=number? exponment 0) 1)
	((=number? exponment 1) base)
	((and (number? base) (number? exponment))
	 (fast-expt base exponment))
	(else (list base '^ exponment))))

(define (expn? e)
  (and (pair? e) (eq? (cadr e) '^)))

(define (base expn) (car expn))
(define (exponment expn) (caddr expn))

(define (sum? x) (and (pair? x) (eq? '+ (car x))))
(define (addebd s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? '* (car x))))

(define (multi1 p) (cadr p))
(define (multi2 p) (caddr p))

(define (deriv expr var)
  (cond ((number? expr) 0)
	((var? expr)
	 (if (same-var? expr var) 1 0))
	((sum? expr)
	 (make-sum (deriv (addebd expr) var)
		   (deriv (augend expr) var)))
	((product? expr)
	 (make-sum
	   (make-product (multi1 expr) (deriv (multi2 expr) var))
	   (make-product (deriv (multi1 expr) var) (multi2 expr))))
	((expn? expr)
	 (let ((u (base expr))
	       (n (exponment expr)))
	   (make-product
	     (make-product n (make-expn u (- n 1)))
	     (deriv u n))))
	(else
	  (error "deriv: error expr!"))))

(deriv '(+ (* 4 (* x x)) (* 3 x) 5) 'x)
(deriv '(* x (* x x)) 'x)
(make-product 'x 'x)
