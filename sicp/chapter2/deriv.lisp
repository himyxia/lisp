;; version 1
(define (deriv expr var)
  (cond ((number? expr) 0)
		((variable? expr) (if (is-the-same-variable? expr var) 1 0))
		((sum? expr)
		 (make-sum (deriv (addend expr) var)
				   (deriv (augend expr) var)))
		((product? expr)
		 (make-sum (make-product (multipler expr)
								 (deriv (multiplicand expr) var))
				   (make-product (deriv (multipler expr))
								 (multiplicand expr))))
		;; add more operation
		(else (error "fuck you"))))


;; version 2
(define (deriv expr var)
  (cond ((number? expr) 0)
		((variable? expr) (if (is-the-same-variable? expr var) 1 0))
		(else ((get 'deriv (operator expr)) (operand expr) var))))

(define (operator expr)
  (car expr))

(define (operands expr)
  (car (cdr expr)))

(define (add-tag tag content)
  (car tag content))

(define (make-sum-exp a b)
  (add-tag 'sum (cons a b)))

(define (make-product-exp a b)
  (add-tag 'product (cons a b)))

(define (install-sum-support)
  (define (deriv-sum expr var) 
	(make-sum (deriv (car expr) var) 
			  (deriv (cdr expr) var)))

  (put 'deriv 'sum deriv))

(define (install-product-support)
  (define (deriv-product expr var) 
	  (make-sum (make-product (deriv (car expr) var) (cdr expr)))
				(make-procudt (car expr) (deriv (cdr expr))))

  (put 'deriv 'product deriv))

;; exercise, 2.73.d
(define (deriv expr var)
  (cond ((number? expr) 0)
		((variable? expr) (if (is-the-same-variable? expr var) 1 0))
		(else ((get (operator expr) 'deriv) (operand expr) var))))

(define (operator expr)
  (car expr))

(define (operands expr)
  (car (cdr expr)))

(define (add-tag tag content)
  (car tag content))

(define (make-sum-exp a b)
  (add-tag 'sum (cons a b)))

(define (make-product-exp a b)
  (add-tag 'product (cons a b)))

(define (install-sum-support)
  (define (deriv-sum expr var) 
	(make-sum (deriv (car expr) var) 
			  (deriv (cdr expr) var)))

  (put 'sum 'deriv deriv))

(define (install-product-support)
  (define (deriv-product expr var) 
	  (make-sum (make-product (deriv (car expr) var) (cdr expr)))
				(make-procudt (car expr) (deriv (cdr expr))))

  (put 'product 'deriv deriv))
