#lang sicp

(define (call-pol)
  (error "pol_coming!"))

(define (make-account balance pwd)
  (let ((pwd_err_count 0))
   (lambda (input_pwd m amount)
    (cond ((not (eq? input_pwd pwd))
           (begin
             (set! pwd_err_count (+ pwd_err_count 1))
             (if (>= pwd_err_count 7)
                 (call-pol)
                 "Incorrect password")))
          ((eq? m 'withdraw)
           (if (< balance amount)
               "balance < amount"
               (begin
                 (set! balance (- balance amount))
                 balance)))
          ((eq? m 'deposit)
           (begin (set! balance (+ balance amount))
                  balance))
          (else "Incorrect operation")))))

(define acc (make-account 142 'abc))
(acc 'abc 'withdraw 45)
(acc 'abc 'withdraw 45)
(acc 'acc 'withdraw 45)
(acc 'azc 'withdraw 45)
(acc 'abc 'deposit 23)
(acc 'abc 'deposit 45)
(acc 'abc 'withdraw 45)
(acc 'abc 'withdraw 45)
(acc 'abc 'deposit 99)
(acc 'abc 'withdraw 45)
(acc 'azz 'withdraw 45)
(acc 'abc 'deposit 45)
(acc 'abc 'withdraw 111)
(acc 'ac 'deposit 45)
(acc 'bc 'deposit 45)
(acc 'ab 'deposit 45)
(acc 'azz 'withdraw 45)