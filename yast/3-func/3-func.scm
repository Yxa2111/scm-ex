(cd "/home/yxa/scheme/yast/3-func")
(load "hello.scm")

vhello
fhello
(fhello)
1

(load "farg.scm")

(hello "yxa")

(sum3 3 4 5)
;ex1
(define (add1 num) (+ num 1))
(define (decre1 num) (- num 1))

(decre1 (add1 4))
;ex2
(define pi (* 4 (atan 1.0)))

(define (deg2rad deg) (* pi (/ deg 180.0)))
(define (distance speed time) (* speed time))
(define gravity 9.8)
;(define (ti v))

(deg2rad 180)
