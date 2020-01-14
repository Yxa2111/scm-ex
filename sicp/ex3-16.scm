#lang planet neil/sicp

;#lang racket
(define (count-pairs pair)
  (define (count-proc pair visited)
    (cond ((not (pair? pair))
           (cons visited 0))
          ((found? pair visited)
           (cons visited 0))
          (else
           (let* ((left-res (count-proc (car pair) (cons pair visited)))
                  (right-res (count-proc (cdr pair) (car left-res))))
             (cons (car right-res)
                   (+ 1 (cdr left-res)
                      (cdr right-res)))))))
  (define (found? pair visited)
    (define (iter vis)
      (cond ((null? vis) #f)
            ((eq? (car vis) pair) #t)
            (else (iter (cdr vis)))))
    (iter visited))
  (cdr (count-proc pair '())))

(count-pairs (cons (list 'a 'b) (list 'a 'b)))

(define c (list 'ptr-to-a))
(define pairs (list c c))
(set-car! c c)

(count-pairs pairs)
pairs