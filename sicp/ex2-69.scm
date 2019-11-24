#lang racket

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? 'leaf (car object)))

(define (symbol-leaf leaf)
  (cadr leaf))

(define (weight-leaf leaf)
  (caddr leaf))

(define (make-code-tree left right)
  (list left right
        (list (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols obj)
  (if (leaf? obj)
      (symbol-leaf obj)
      (caddr obj)))

(define (weight obj)
  (if (leaf? obj)
      (weight-leaf obj)
      (cadddr obj)))

(define (decode bits tree)
  (define (proc bits current)
    (if (null? bits)
      '()
      (let ((next-branch
             (choose-branch (car bits) current)))
        (if (leaf? next-branch)
            (cons (symbol-leaf next-branch)
                  (proc (cdr bits) tree))
            (proc (cdr bits) next-branch)))))
  (proc bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit:" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else
         (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set
         (make-leaf (car pair) (cadr pair))
         (make-leaf-set (cdr pairs))))))

;O(n^2)

(define sample-t
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))
(define sample-bits '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-bits sample-t)

(define (encode message tree)
  (define (encode-symbol sym br)
    (cond ((null? br) #f)
          ((leaf? br)
           (if (eq? (symbols br) sym) '() #f))
          (else
           ((lambda (left-res)
             (if (eq? left-res '())
                 (cons 0 '())
                 (cons 1 (encode-symbol sym (right-branch br))))) (encode-symbol sym (left-branch br))))))
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(encode '(A D A B B C A) sample-t)

(define (generate-huffman-tree pairs)
  (define (successive-merge lst)
    (define (proc lst len)
      (if (> len 1)
          (let* ((elem1 (car lst))
                 (elem2 (cadr lst))
                 (new-elem (make-code-tree elem1 elem2))
                 (new-lst (adjoin-set new-elem (cddr lst))))
            (proc new-lst (- len 1)))
          lst))
    (proc lst (length lst)))
  (car (successive-merge (make-leaf-set pairs))))
(generate-huffman-tree '((A 12) (B 10) (C 1) (D 1) (E 13) (F 22)))
(define t2 (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))))

sample-t
t2