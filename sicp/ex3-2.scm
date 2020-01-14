#lang sicp

(define (make-monitored f)
  (let ((count 0))
    (lambda (m)
      (if (eq? m 'how-many-calls?)
          count
          (begin (set! count (+ count 1))
                 (f m))))))

(define s (make-monitored sqrt))

(s 100)
(s 'how-many-calls?)
(s 20)
;(s 'how-manu-calls?)

(s 10000)
;(s 'how-manu-calls?)

(s 1000)
(s 'how-many-calls?)
