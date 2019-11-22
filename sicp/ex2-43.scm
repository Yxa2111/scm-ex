;#lang racket/load
(load "/home/yxaa/scm-ex/sicp/ex2-40.scm")

(define (flatmap proc seq)
  (accu append '() (map proc seq)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list '())
	(filter
	  (lambda (positions) (safe? positions))
	  (flatmap
	    (lambda (rest-of-queens)
	      (map
		(lambda (new-row)
		  (adjoin-position new-row k rest-of-queens))
		(make-seq 1 board-size)))
	    (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (adjoin-position row col seq)
  (cons (cons row col) seq))

(define (safe? positions)
  (define (loop point seq)
    (if (null? seq)
	#t
	(if (not (point-fit (car seq) point))
	    #f
	    (loop point (cdr seq)))))
     (loop (car positions) (cdr positions)))

(define (point-fit p1 p2)
  (let ((x1 (car p1)) (y1 (cdr p1))
	(x2 (car p2)) (y2 (cdr p2)))
    (not (or
	   (= x1 x2)
	   (= y1 y2)
	   (= (abs (- x1 x2))
	      (abs (- y1 y2)))))))

(define (lst-len lst)
  (accu (lambda (item num) (+ num 1)) 0 lst))

(lst-len (queens 8))
