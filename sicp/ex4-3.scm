#lang planet neil/sicp

(define apply-in-underlying-scheme apply)

(define undefined '**undefined)
(define (undefined? exp) (eq? exp undefined))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

;4.1.1

(define (analyze exp)
  (cond ((self-evaluating? exp) (anlyz-self-evaluating exp))
        ((undefined? exp) (lambda (env succ fail) (succ undefined fail)))
        ((amb? exp) (analyze-amb exp))
        ((variable? exp) (anlyz-variable-value exp))
        ((quoted? exp) (anlyz-quoted exp))
        ((assignment? exp) (anlyz-assignment exp))
        ((definition? exp) (anlyz-definition exp))
        ((if? exp) (anlyz-if exp))
        ((lambda? exp) (anlyz-lambda exp))
        ((begin? exp) (anlyz-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (anlyz-let->combination exp))
        ((let*? exp) (anlyz-let*->let exp))
        ((letrec? exp) (anlyz-letrec exp))
        ((and? exp) (anlyz-and exp))
        ((or? exp) (anlyz-or exp))
        ((not? exp) (anlyz-not exp))
        ((application? exp) (anlyz-app exp))
        (else
         (error "Unknown expr type -- analyze" exp))))

(define (anlyz-self-evaluating exp) (lambda (env succ fail) (succ exp fail)))
(define (anlyz-variable-value exp)
  (lambda (env succ fail)
    (succ
     (lookup-variable-value exp env)
     fail)))
(define (anlyz-quoted exp)
  (lambda (env succ fail)
    (succ (text-of-quotation exp) fail)))

(define (anlyz-assignment exp)
  (let ((var (assignment-variable exp))
        (val-proc (analyze (assignment-value exp))))
  (lambda (env succ fail)
    (val-proc
     env
     (lambda (val fail2)
       (let ((old-val (lookup-variable-value var env)))
         (set-variable-value! var val env)
         (succ env
               'ok
               (lambda ()
                 (set-variable-value! var old-val env)))))
     fail))))

(define (anlyz-definition exp)
  (let ((var (definition-variable exp))
        (val-proc (analyze (definition-value exp))))
  (lambda (env succ fail)
    (val-proc
     env
     (lambda (val fail2)
       (define-variable! var val env)
       (succ 'ok fail2))
     fail))))

(define (anlyz-if exp)
  (let ((predicate-proc (analyze (if-predicate exp)))
        (consequent-proc (analyze (if-consequent exp)))
        (alternative-proc (analyze (if-alternative exp))))
    (lambda (env succ fail)
      (predicate-proc env
                      (lambda (pp-val fail2)
                        (if (true? pp-val)
                            (consequent-proc env succ fail2)
                            (alternative-proc env succ fail2)))
                      fail))))

(define (anlyz-lambda exp)
  (let ((para (lambda-parameters exp))
        (body-proc (anlyz-sequence (lambda-body exp))))
    (lambda (env succ fail)
      (succ
       (make-procedure
        para
        body-proc
        env)
       fail))))

(define (anlyz-let->combination exp)
  (if (named-let? exp)
      (let* ((l (make-lambda
                 (let-parameters (let-arglist exp))
                 (let-body exp)))
             (name (let-named exp))
             (ll (make-lambda
                  (let-parameters (let-arglist exp))
                  (cons (make-define name l) (let-body exp)))))
        (anlyz-app
         (cons
          ll
          (let-arguments (let-arglist exp)))))
      (anlyz-app
       (cons
        (make-lambda
         (let-parameters (let-arglist exp))
         (let-body exp))
        (let-arguments (let-arglist exp))))))

(define (anlyz-let*->let exp)
  (let ((args (let-arglist exp)))
  (if (or (null? args) (let-last-arg? args))
      (anlyz-let->combination (make-let args (let-body exp)))
      (let ((var (let-first-para args))
            (val (let-first-arg args)))
        (anlyz-let->combination
         (make-let
          (list (list var val))
          (list (make-let*
           (cdr args)
           (let-body exp)))))))))

(define (anlyz-letrec exp)
  (define (add-defines args new-body)
    (if (null? args)
        new-body
        (add-defines (cdr args)
                     (cons (make-define (let-first-para args) (let-first-arg args)) new-body))))
  (anlyz-app
   (list
    (make-lambda
     '()
     (add-defines (let-arglist exp) (let-body exp))))))

(define (anlyz-and exp)
  (define (iter vprocs last env succ fail)
    (if (null? vprocs)
        (succ last fail)
        ((car vprocs) env
                      (lambda (val fail2)
                        (if (false? val)
                            (succ false fail2)
                            (iter (cdr vprocs) val env succ fail2)))
                      fail)))
  (let ((vprocs (map analyze (cdr exp))))
    (lambda (env succ fail)
      (iter vprocs true env succ fail))))

(define (anlyz-or exp)
  (define (iter vprocs env succ fail)
    (if (null? vprocs)
        (succ false fail)
        ((car vprocs) env
                      (lambda (val fail2)
                        (if (not (false? val))
                            (succ val fail2)
                            (iter (cdr vprocs) env succ fail2)))
                      fail)))
  (let ((vprocs (map analyze (cdr exp))))
    (lambda (env succ fail)
      (iter vprocs env succ fail))))

(define (anlyz-not exp)
  (define vproc (analyze (cadr exp)))
  (lambda (env succ fail)
    (vproc env
           (lambda (val fail2)
             (if (false? val)
                 (succ true fail2)
                 (succ false fail2)))
           fail)))

(define (anlyz-sequence exps)
  (define (step-by-step s1 s2)
    (lambda (env succ fail)
      (s1 env
          (lambda (s1-val fail2)
            (s2 env succ fail2))
          fail)))
  (define (iter first rest)
    (if (null? rest)
        first
        (iter (step-by-step
               first
               (car rest))
              (cdr rest))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "empty proc -- anlyz-sequence" exps)
        (iter (car procs) (cdr procs)))))

(define (anlyz-app exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succ fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs env
                         (lambda (args fail3) (execute-app proc args succ fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succ fail)
  (if (null? aprocs)
      (succ '() fail)
      (let ((ap (car aprocs)))
        (ap env
            (lambda (arg fail2)
              (get-args (cdr aprocs)
                        env
                        (lambda (rest-args fail3)
                          (succ (cons arg rest-args)
                                fail3))
                        fail2))
            fail))))

(define (execute-app proc args succ fail)
  (cond ((primitive-procedure? proc)
         (succ (apply-primitive-procedure proc args) fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
           (procedure-parameters proc)
           args
           (procedure-environment proc))
          succ fail))))

(define (new-eval exp env)
  ((analyze exp) env))

; 4.1.2
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        ((boolean? exp) true)
        (else false)))

(define (variable? exp)
  (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp) ; var
      (caadr exp) ;lambda
      ))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))

(define (make-define var val) (list 'define var val))

; ex 4.16
(define (scan-out-defines body)
  (define (scan body let-args def-list)
    (cond ((null? body) (cons let-args def-list))
          ((tagged-list? (car body) 'define)
           (scan (cdr body)
                 (cons (list (definition-variable (car body)) undefined) let-args)
                 (cons (list (definition-variable (car body)) (definition-value (car body))) def-list)))
          (else (scan (cdr body) let-args def-list))))
  (define (flip list new-list)
    (if (null? list)
        new-list
        (flip (cdr list) (cons (car list) new-list))))
  (define (set!-attach def-list body)
    (if (null? def-list)
        body
        (set!-attach (cdr def-list)
                     (cons
                      (cons 'set! (car def-list))
                      body))))
  (define (remove-defines body)
    (define (iter body new-body)
      (cond ((null? body) (flip new-body '()))
            ((tagged-list? (car body) 'define)
             (iter (cdr body) new-body))
            (else (iter (cdr body) (cons (car body) new-body)))))
    (iter body '()))
  (let ((res (scan body '() '())))
    (let ((let-args (car res))
          (def-list (cdr res)))
      (if (eq? let-args '())
          body
          (list (make-let let-args
                          (set!-attach
                           def-list
                           (remove-defines body))))))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp)) ; not caddr! need '()

(define (make-lambda parameters body) (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (null? (cdddr exp))
      'false
      (cadddr exp)))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))
(define (not? exp) (tagged-list? exp 'not))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? exp) (null? (cdr exp)))

(define (first-exp exp) (car exp))

(define (rest-exps exp) (cdr exp))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? exp) (null? exp))

(define (first-operand exp) (car exp))

(define (rest-operand exp) (cdr exp))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-caluses exp) (cdr exp))

(define (cond-else-caluse? exp)
  (eq? (cond-predicate exp) 'else))

(define (cond-predicate exp) (car exp))

(define (cond-actions exp) (cdr exp))

(define (cond->if exp) (expand-clauses (cond-caluses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-caluse? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "else clause isn't last --- expand-clauses" rest))
            (make-if 
             (cond-predicate first)
             (sequence->exp (cond-actions first))
             (expand-clauses rest))))))

; ex 4.6

(define (let? exp) (tagged-list? exp 'let))

(define (let-arglist exp)
  (if (pair? (cadr exp))
      (cadr exp)
      (if (pair? (caddr exp))
          (caddr exp)
          (error "ill-formed let synatx" exp))))

(define (named-let? exp) (symbol? (cadr exp)))
(define (let-named exp) (if (named-let? exp) (cadr exp) (error "not let named!" exp)))
(define (let-first-para args) (caar args))
(define (let-first-arg args) (cadar args))
(define (let-last-arg? args) (null? (cdr args)))
  
(define (let-parameters args)
  (if (null? args)
      '()
      (cons (let-first-para args) (let-parameters (cdr args)))))

(define (let-arguments args)
  (if (null? args)
      '()
      (cons (let-first-arg args) (let-arguments (cdr args)))))

(define (let-body exp)
  (if (named-let? exp)
      (cdddr exp)
      (cddr exp)))

(define (make-let arglist body) (cons 'let (cons arglist body)))

(define (let*? exp) (tagged-list? exp 'let*))

(define (make-let* arglist body) (cons 'let* (cons arglist body)))

; ex 4.20

(define (letrec? exp) (tagged-list? exp 'letrec))

; 4.1.3

(define (true? x) (not (false? x)))

(define (false? x) (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (define (iter variables values)
    (if (null? variables)
        '()
        (let ((var (car variables))
              (val (car values)))
          (cons
           (cons var val)
           (make-frame (cdr variables) (cdr values))))))
  ;  (if (and (null? variables) (null? values))
  ;      '('())
  (iter variables values))

(define (frame-variables frame)
  (if (null? frame)
      '()
      (cons (car frame) (frame-variables (cdr frame)))))

(define (frame-values frame)
  (if (null? frame)
      '()
      (cons (cdr frame) (frame-variables (cdr frame)))))

(define (add-binding-to-frame! var val frame)
  (cons (cons var val) frame))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (if (null? vars)
          base-env
          (cons (make-frame vars vals) base-env))
      (error "vars vals length doesn't match --- extend-env" vars vals)))

(define (lookup-variable-value var env)
  (define (scan frame)
    (cond ((null? frame)
           (lookup-variable-value var (enclosing-environment env)))
          ((eq? var (caar frame))
           (let ((value (cdar frame)))
             (if (eq? undefined value)
                 (error (caar frame) "undefined; cannot use before initialization")
                 value)))
          (else (scan (cdr frame)))))
  (if (eq? env the-empty-environment)
      (error "unbound variable" var))
  (scan (first-frame env)))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar frame)) (set-cdr! (car frame) val))
            (else (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
        (error "unbound variable -- set!")
        (scan (first-frame env))))
  (env-loop env))

(define (define-variable! var val env)
  (define (scan frame)
    (cond ((null? frame)
           (let ((new-frame (cons (cons var val) (first-frame env))))
             (set-car! env new-frame)))
          ((eq? var (caar frame)) (set-cdr! (car frame) val))
          (else (scan (cdr frame)))))
  (scan (first-frame env)))

; 4.1.4

(define (square n) (* n n))

(define primitive-procedures
  (list (cons 'car car)
        (cons 'cdr cdr)
        (cons 'cadr cadr)
        (cons 'caddr caddr)
        (cons 'cadddr cadddr)
        (cons 'cddr cddr)
        (cons 'cdddr cdddr)
        (cons 'cddddr cddddr)
        (cons 'cons cons)
        (cons 'null? null?)
        (cons '+ +)
        (cons '- -)
        (cons '* *)
        (cons '/ /)
        (cons '= =)
        (cons '< <)
        (cons '> >)
        (cons '>= >=)
        (cons '<= <=)
        (cons 'abs abs)
        (cons 'even? even?)
        (cons 'modulo modulo)
        (cons 'square square)
        (cons 'exact->inexact exact->inexact)
        (cons 'list list)
        (cons 'append append)
        (cons 'pair? pair?)
        (cons 'newline newline)
        (cons 'display display)
        (cons 'number? number?)
        (cons 'symbol? symbol?)
        (cons 'eq? eq?)
        (cons 'null? null?)
        (cons 'length length)
        (cons 'quotient quotient)
        (cons 'set-cdr! set-cdr!)
        (cons 'set-car! set-car!)
        (cons 'member member)))

(define (primitive-procedure-names)
  (define (iter proc)
    (if (null? proc)
        '()
        (cons (caar proc) (iter (cdr proc)))))
  (iter primitive-procedures))

(define (primitive-procedure-objects)
  (define (iter proc)
    (if (null? proc)
        '()
        (cons (list 'primitive (cdar proc)) (iter (cdr proc)))))
  (iter primitive-procedures))


(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define (primitive-procedure? proc) (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))


(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succ fail)
      (define (next procs)
        (if (null? procs)
            (fail)
            ((car procs) env
                         succ
                         (lambda () (next (cdr procs))))))
      (next cprocs))))

(define input-prompt ";;; Amb-eval input:")
(define output-prompt ";;; Amb-eval value:")
(define (prompt-for-input str) (newline) (newline) (display str) (newline))
(define (announce-output str) (newline) (display str) (newline))

(define (user-input object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     'env))
      (display object)))

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; starting new problem")
            (ambeval input the-global-environment
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-input val)
                       (internal-loop next-alternative))
                     (lambda ()
                       (announce-output "no more solutions")
                       (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display "There is no current problem")
     (driver-loop))))

(driver-loop)