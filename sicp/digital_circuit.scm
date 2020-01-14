#lang planet neil/sicp

;wire
(define (make-wire)
  (let ((signal-value 0)
        (action-procedure '()))
    (define (set-signal! new-value)
      (if (not (= new-value signal-value))
          (begin
            (set! signal-value new-value)
            (call-each action-procedure))
          'done))
    (define (add-action-procedure new-proc)
      (set! action-procedure
            (cons new-proc action-procedure))
      (new-proc))
    (define (dispatch m)
      (cond ((eq? m 'set-signal!) set-signal!)
            ((eq? m 'get-signal) signal-value)
            ((eq? m 'add-action!) add-action-procedure)
            (else (error "wire -- invaild message"))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire new-action)
  ((wire 'add-action!) new-action))

;gate
(define (gate-update-logical input-list out delay logical-procedure)
  (define (update-output)  
    (let ((new-value (logical-procedure
                     (map (lambda(i)
                            (get-signal i))
                          input-list))))
      (after-delay delay
                   (lambda()
                     (set-signal! out new-value)))))
  (for-each (lambda(i)
              (add-action! i update-output))
            input-list)
  'done)

(define (inverter in out)
  (gate-update-logical (list in) out inverter-delay logical-not))

(define (logical-not value-list)
  (let ((value (car value-list)))
    (cond ((= value 0) 1)
          ((= value 1) 0)
          (else 'logiacl-not-value-not-found))))

(define (and-gate in1 in2 out)
  (gate-update-logical (list in1 in2) out and-gate-delay logical-and))

(define (logical-and value-list)
  (let iter((list value-list))
    (if (null? list) 1
        (let ((val (car list)))
          (cond ((= val 0) 0)
                ((= val 1) (iter (cdr list)))
                (else 'logical-and-input-err))))))

(define (or-gate in1 in2 out)
  (gate-update-logical (list in1 in2) out or-gate-delay or-logical))

(define (or-logical value-list)
  (let iter((list value-list))
    (if (null? list) 0
        (let ((val (car list)))
          (cond ((= val 1) 1)
                ((= val 0) (iter (cdr list)))
                (else 'logical-or-input-err))))))
;adder
(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in s c-out)
  (let ((s1 (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s1 c1)
    (half-adder a s1 s c2)
    (or-gate c1 c2 c-out)
    'ok))

;queue
(define (make-queue)
  (let* ((front-ptr (cons 'empty-item '()))
         (rear-ptr front-ptr))
    (define (empty-queue?) (eq? front-ptr rear-ptr))
    (define (insert-queue! item)
      (let ((new-elem (list item)))
        (begin (set-cdr! rear-ptr new-elem)
               (set! rear-ptr new-elem)
               dispatch)))
    (define (delete-queue!)
      (if (empty-queue?)
          'queue-empty
          (let ((deleted-item (cadr front-ptr)))
            (begin (set! front-ptr (cdr front-ptr))
                   (set-car! front-ptr 'empty-item)
                   deleted-item))))
    (define (front-item) (cadr front-ptr))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'front-item) front-item)
            (else (error "queue -- invaild-message"))))
  dispatch))

(define (empty-queue? q) ((q 'empty-queue?)))
(define (insert-queue! q item) ((q 'insert-queue!) item))
(define (delete-queue! q) ((q 'delete-queue!)))
(define (front-item q) ((q 'front-item)))

;time-segment
(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time segment)
  (car segment))
(define (segment-queue segment)
  (cdr segment))

;agdenda
(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda)
  (car (segments agenda)))
(define (rest-segments agenda)
  (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  
  (define (make-new-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))

  (define (add-to-segments! segments)
    (let ((segment (car segments))
          (rest (cdr segments)))
      (if (= time (segment-time segment))
          (insert-queue!
            (segment-queue segment)
            action)
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons
                (make-new-segment time action)
                rest))
              (add-to-segments! rest)))))
;body
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons
          (make-new-segment time action)
          segments))
        (add-to-segments! segments))))


(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "agenda is empty")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-item (segment-queue first-seg)))))

;delay
(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-val = ")
                 (display (get-signal wire)))))

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)

(half-adder input-1 input-2 sum carry)
(set-signal! input-1 1)
(propagate)

(set-signal! input-2 1)
(propagate)

(set-signal! input-1 0)
(set-signal! input-2 0)
(propagate)
