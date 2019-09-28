;ex1
(define (lst-x2 lst)
  (map (lambda (x) (* 2 x)) lst))

(lst-x2 '(1 3 5 7 9))

(define (lsts-decr ls1 ls2)
  (map (lambda (x y) (- x y))
       ls1 ls2))

(lsts-decr '(9 8 7 6 5) '(3 2 1 0 4))
