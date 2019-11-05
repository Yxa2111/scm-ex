(define zero
  (lambda (f)
    (lambda (x) x)))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

(define one
  (lambda (f)
    (lambda (x) (f x))))

(define two
  (lambda (f)
    (lambda (x) (f (f x)))))

(define three
  (lambda (f)
    (lambda (x) (f (f (f x))))))

(define p1 (add-1 two))
((p1 (lambda (x) (+ 1 x))) 0)

(define (lambda-plus p1 p2)
  (lambda (f)
    (lambda (x) 
      ((p1 f) ((p2 f) x)))))

(define p2 (lambda-plus three two))
((p2 (lambda (x) (+ 1 x))) 0)
