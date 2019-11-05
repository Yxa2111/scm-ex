;ex1
(define (my-len list)
  (if (null? list)
    0 (+ (my-len (cdr list)) 1)))

(my-len '(1 2 3 44 5 6 7 8 9))
(my-len '())

(define (lst-erase e ls)
  (if (null? ls) '()
   (if (eqv? e (car ls)) 
    (lst-erase e (cdr ls))
    (cons (car ls) (lst-erase e (cdr ls))))))

(define l1 '(3 1 4 3 2 1 2 3 4 5))
(car l1)
(cdr l1)
(lst-erase 1 l1)
(lst-erase 4 l1)
(lst-erase 5 l1)
(lst-erase 3 l1)
(lst-erase 3 '(1 2))
(lst-erase 1 '())

(define (lst-sum lst)
  (if (null? lst)
    0
    (+ (car lst) (lst-sum (cdr lst)))))

(lst-sum '(1 2 3 4 5))
(lst-sum '())

(define (lst-find e lst)
  (if (null? lst)
    #f
    (if (eqv? e (car lst))
      0
      (let ((pos (lst-find e (cdr lst))))
	(and pos (+ 1 pos))))))

(lst-find 5 l1)
(lst-find 4 l1)
(lst-find 100 l1)

;ex2
(define (lst-rev lst ans)
  (if (null? lst)
    ans
    (lst-rev (cdr lst) (cons (car lst) ans))))

(lst-rev '(1 2 3 4 5) '())
(lst-rev '() '())

(define (str2int str)
  (lst-c2i (string->list str) 0))

(define (lst-c2i lst n)
  (if (null? lst)
    n
    (lst-c2i (cdr lst)
	     (+ (* n 10) (- (char->integer(car lst)) 48)))))

(str2int "1232111")

(define (lst-reverse lst)
  (lst-rev lst '()))

;ex3
(define (lst-rm-let e lst)
  (let loop((l lst) (p '()))
    (if (null? l)
      (lst-reverse p)
      (let ((h (car l)) (t (cdr l)))
	(if (eqv? h e)
	  (loop t p)
	  (loop t (cons h p)))))))

(lst-rm-let 1 '(2 3 4 1 2 ))

(define (lst-find-let e lst)
  (let loop((l lst))
    (if (null? l)
      #f
	(if (eqv? (car l) e)
	  0
	  (let ((pos (loop (cdr l))))
	    (and pos (+ 1 pos)))))))

(lst-find-let 1 '(2 3 4 5 6 1 2))

(define (range-let n)
  (let loop((lst '()) (i (- n 1)))
    (if (< i 0)
      lst
      (loop (cons i lst) (- i 1)))))

(range-let 4)
(range-let 0)
(range-let -1)
(range-let 100)

;ex4

