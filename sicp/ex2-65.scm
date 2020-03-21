#lang planet neil/sicp

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (elem-of-set? x set)
  (cond ((null? set) #f)
	((= x (entry set)) #t)
	((< x (entry set))
	 (elem-of-set? x (left-branch set)))
	(else (elem-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
	((= x (entry set)) set)
	((< x (entry set))
	 (make-tree
	   (entry set)
	   (adjoin-set x (left-branch set))
	   (right-branch set)))
	(else
	  (make-tree
	    (entry set)
	    (left-branch set)
	    (adjoin-set x (right-branch set))))))

(define (rev-lst lst)
  (define (loop ret lst)
    (if (null? lst)
        ret
        (loop (cons (car lst) ret) (cdr lst))))
  (loop '() lst))

(define (set->list set)
  (define (proc set lst)
    (if (null? set)
	lst
      	(proc
	  (right-branch set)
	  (cons
           (entry set)
	   (proc (left-branch set) lst)))))
  (rev-lst (proc set '())))

(define (make-set lst)
  (define (iter set lst)
    (if (null? lst)
	set
	(iter (adjoin-set (car lst) set) (cdr lst))))
  (iter '() lst))

(define s1 (make-set '(4 5 1 11 33 2 44 1 22 98 45 63 21 78 99)))
s1
(define x (set->list s1))

(define (list->set lst)
  (define (proc elts n)
    (if (= n 0)
        (cons '() elts)
        (let* ((left-size (quotient (- n 1) 2))
                 (left-result (proc elts left-size))
                 (left-tree (car left-result))
                 (left-elts (cdr left-result))
                 (right-size (- n (+ left-size 1)))
                 (this-entry (car left-elts))
                 (right-result (proc (cdr left-elts) right-size))
                 (right-tree (car right-result))
                 (remaining-elts (cdr right-result)))
          (cons (make-tree this-entry left-tree right-tree) remaining-elts))))
  (car (proc lst (length lst))))

(set->list (list->set (set->list s1)))

(define (union-set set1 set2)
  (define (merge lst1 lst2 res-lst)
    (cond ((null? lst1)
           (append lst2 res-lst))
          ((null? lst2)
           (append lst1 res-lst))
          ((< (car lst1) (car lst2))
           (merge (cdr lst1) lst2 (cons (car lst1) res-lst)))
          (else
           (merge lst1 (cdr lst2) (cons (car lst2) res-lst)))))
  (let* ((lst1 (set->list set1))
         (lst2 (set->list set2))
         (res-lst (rev-lst (merge lst1 lst2 '()))))
  (list->set res-lst)))

(define (intersection-set set1 set2)
  (define (same-filter lst1 lst2 res-lst)
    (cond ((or (null? lst1) (null? lst2))
           res-lst)
          ((= (car lst1) (car lst2))
           (same-filter (cdr lst1) (cdr lst2) (cons (car lst1) res-lst)))
          ((< (car lst1) (car lst2))
           (same-filter (cdr lst1) lst2 res-lst))
          (else
           (same-filter lst1 (cdr lst2) res-lst))))
  (let* ((lst1 (set->list set1))
         (lst2 (set->list set2))
         (res-lst (rev-lst (same-filter lst1 lst2 '()))))
  (list->set res-lst)))

(define s2 (make-set '(11 13 15 17 9 7 5 3 1 2 4 6)))
(define s3 (make-set '(2 8 6 4 10 14 16 12)))
(define s4 (union-set s2 s3))
(set->list (intersection-set s2 s3))
(set->list (intersection-set s2 s4))
(set->list (intersection-set s4 s3))