#lang planet neil/sicp

(define (make-wire)
  (let ((signal-value 0)
        (action-procedure '()))
    (define (set-signal! new-value)
      (if (not (= new-value signal-value))
          (begin
            (set! signal-value new-value)
            (for-each action-procedure))
          'done))
    (define (add-action-procedure new-pro)
      (set! action-procedure
            (cons new-pro action-procedure)))
    (define (dispatch m)
      (cond ((eq? m 'set-signal!) set-signal!)
            ((eq? m 'get-signal) action-procedure)
            ((eq? m 'add-action!) add-action-procedure)
            (else 'message-not-found)))
    dispatch))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire new-action)
  ((wire 'add-action!) new-action))

