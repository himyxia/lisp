(define (eval exp env)
  (cond ((self-evaluation? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
                  (make-procedure (lambda-parameters exp)
                                  (lambda-body exp)
                                  env))
        ((begin? exp)
                 (eval-sequence (begin-actions exp) env)) 
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
                       (apply (eval (operator exp) env)
                              (list-of-values (operands exp) env)))
        (elsee (error "Unknown expression type - EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure) 
          (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure) 
          (eval-sequence
            (procedure-body procedure)
            (extend-environment
              (procedure-parameters procedure)
              arguments
              (procedure-environment procedure)))) 
        (else (error "Unknow procedure type - APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (firsts-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;; 4.1 start 
(define (list-of-values-from-left-to-right exps env)
  (if (no-operands? exps)
      '()
      (let ((first (eval (first-operand exps) env)))
        (cons first (list-of-values (rest-operands exps) env)))))
      
(define (list-of-values-from-right-to-left exps env)
  (if (no-operands? exps)
      '()
      (let ((rest (list-of-values (rest-operands exps) env)))
        (cons (eval (first-operand exps) env) rest))))
;; 4.1 end 
      

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))



(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))


(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-val exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)


(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))


(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (car (cdr exp)))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
  
  
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (car (cdr exp)))
(define (assignment-value exp) (car (cdr (cdr exp))))


(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (car (cdr exp)))
      (car (cdr exp))
      (car (cdr (cdr exp)))))
    
(define (definition-value exp)
  (if (symbol? (car (cdr exp)))
      (car (cdr (cdr exp)))
      (make-lambda (cdr (car (car (cdr exp))))
                   (cdr (cdr exp)))))
                 
                 
(define (lambda? exp)
  (tagged-lists? exp 'lambda))

(define (lambda-parameters exp)
  (car (cdr exp)))

(define (lambda-body exp)
  (cdr (cdr exp)))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (car (cdr exp)))
(define (if-consequent exp) (car (cdr (cdr exp))))

(define (if-alternative exp) 
  (if (not (null? (cdr (cdr (cdr exp)))))
      (car (cdr (cdr (cdr exp))))
      'false))
    
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exp? seq) (cdr seq))


(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operand ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause)
  (car clause))

(define (cond-actions clause)
  (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
          (if (cond-else-clause? first)
              (if (null? rest)
                  (sequence-exp (cond-actions first))
                  (error "ELSE clause isn't last-COND-IF", clauses))
              (make-if (cond-predicate first)
                       (sequence-exp (cond-actions first))
                       (expand-clauses rest))))))


;; 4.2
