(define (deriv expr var)
  (cond ((number? expr) 0)
		((variable? expr) (if (is-the-same-variable? expr var) 1 0))
		(((get 'deriv (operator expr)) ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-expr-sum a b)
  (add-tag 'sum (cons a b)))

(define (make-expr-product a b)
  (add-tag 'product (cons a b)))

(define (make-expr operator operands)
  (cons operator operands))

(define (deriv-sum expr var)
  (make-sum (deriv (car (operands expr) var)
			(deriv (cdr (operands expr) var)))))

(define (deriv-product expr var)
  (make-sum (make-product (deriv (car (operands expr) var) (cdr (operands expr))))
			(make-procudt (car (operands expr)) (deriv (cdr (operands expr))))))

(define (install-sum)
	(registry 'deriv 'sum deriv-sum))

(define (install-product)
	(registry 'deriv 'product deriv-product))



(define (operator expr)
  (car expr))

(define (operands expr)
  (car (cdr expr)))
