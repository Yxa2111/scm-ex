(define (cube x)
  (* x x x))

(define (sum f a next b c)
  (if (> a b)
    0
    (+ (* c (f a))
       (sum f (next a) next b c))))

(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (add-y a) (+ a (* 2 h)))
  (exact->inexact
    (*
     (+ (sum f a add-y b 2)
        (sum f (+ a h) add-y b 4))
     (/ h 3))))

(integral cube 0 1 100)
(integral cube 0 1 1000)
(integral cube 0 1 10000)
