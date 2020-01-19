#lang planet neil/sicp

(define (car-stream s)
  (car s))

(define (cdr-stream s)
  (force (cdr s)))

(define ones
  (cons-stream 1 ones))

(define integers
  (cons-stream 1 (add-streams integers ones)))

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream (apply proc (map car-stream argstreams)) 
                   (apply stream-map
                          (cons proc (map cdr-stream argstreams))))))

(define (stream-ref n stream)
  (if (= n 0)
      (car-stream stream)
      (stream-ref (- n 1) (cdr-stream stream))))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (print-stream stream n)
  (define (print-num num)
    (display num)
    (display " "))
  (if (= n 0)
      'done
      (begin (print-num (car-stream stream))
             (print-stream (cdr-stream stream) (- n 1)))))

(define factorials (cons-stream 1 (mul-streams integers factorials)))

(print-stream factorials 20)

(define s (cons-stream 1 (add-streams s s)))

(print-stream s 20)

(define (partial-sums stream)
  (define sum-stream
    (cons-stream
     0
     (add-streams sum-stream stream)))
  (cdr-stream sum-stream))

(define ss (partial-sums integers))

(print-stream ss 20)

(define (scale-stream stream factor)
  (stream-map (lambda(x) (* x factor)) stream))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (car-stream s1))
               (s2car (car-stream s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (cdr-stream s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (cdr-stream s2))))
                 (else
                  (cons-stream s1car
                               (merge
                                (cdr-stream s1) (cdr-stream s2)))))))))

(define sss (cons-stream 1
                         (merge (scale-stream sss 2)
                                (merge
                                 (scale-stream sss 3)
                                 (scale-stream sss 5)))))

(print-stream sss 20)

 