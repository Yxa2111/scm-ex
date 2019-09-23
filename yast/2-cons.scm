;ex1
(cons "hi" "everybody")
(cons 0 '())
(cons 1 (cons 1 10))
(cons 1 (cons 10 (cons 100 ()))) ;() or '() is right
(cons #\I (cons "saw" (cons 3 (cons "girls" '()))))
(cons "Sum of" (cons '(1 2 3 4) '("is" 10)))
; (cons 1 2) is not right?
(cdr (cdr '(1 2 3 4)))
;ex2
(car '(0))
(cdr '(0))
(car '((1 2 3) (4 5 6)))
(cdr '(1 2 3 . 4))
(cdr (cons 3 (cons 2 (cons 1 '()))))
(cdr (list 1 2 3 "hello" #\c 4 4 4 "9999999"))
