(define (ti-test n p)
  (newline)
  (display n)
  (start-test n p (runtime)))

(define (start-test n p start-ti)
  (if (p n)
      (report-prime (- (runtime) start-ti))
      #f))

(define (report-prime ti)
  (newline)
  (display "***")
  (display ti)
  #t)
