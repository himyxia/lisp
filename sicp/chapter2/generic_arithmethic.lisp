(define (add x y)
  (apply-generic 'add x y))

(define (sub x y)
  (apply-generic 'sub x y))

(define (mul x y)
  (apply-generic 'mul x y))

(define (div x y)
  (apply-generic 'div x y))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
		((number? datum) 'scheme-number) 
		(else (error "bad tagged datum"))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
		((number? datum) datum) 
		(else (error "bad tagged datum"))))

(define (apply-generic op .args)
  (let ((type-tags (map type-tag args)))
	(let ((proc (get op type-tags)))
	  (if proc 
		(apply proc (map contents args))
		(error "no method for these types" (list op type-tags))))))

(define (attach-tag type-tag contents)
  (if (number? contents) 
	contents 
	(cons type-tag contents)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ordinary number
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-scheme-number-package)
  (define (tag x)
	(attach-tag 'scheme-number x))

  (put 'equ? '(scheme-number scheme-number)
	   (lambda x y) (= x y))

  (put '=zero? 'scheme-number
	   (lambda x) (= x 0))

  (put 'add '(scheme-number scheme-number)
	   (lambda x y) (tag (+ x y)))

  (put 'sub '(scheme-number scheme-number)
	   (lambda x y) (tag (- x y)))

  (put 'mul '(scheme-number scheme-number)
	   (lambda x y) (tag (* x y)))

  (put 'div '(scheme-number scheme-number)
	   (lambda x y) (tag (/ x y)))

  (put 'make 'scheme-number
	   (lambda (x) (tag x)))

  'done)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rational number

(define (make-rational n d)
  ((get 'make 'rational) n d))


(define (install-ration-package)
  (define (tag x) (attach-tag 'ratonal x))

  (put 'equ? '(rational rational) 
	   (lambda (x y) (equ-rat? x y)))

  (put '=zero? 'rational
	   (lambda (x) (=zero-rat? x)))

  (put 'add '(rational rational) 
	   (lambda (x y) (tag (add-rat x y))))

  (put 'sub '(rational rational)
	   (lambda (x y) (tag (sub-rat x y))))

  (put 'mul '(rational rational)
	   (lambda (x y) (tag (mul-rat x y))))

  (put 'div '(rational rational)
	   (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
	   (lambda (n d) (tag (make-rat n d))))

  (define (number x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
	(let ((g (gcd n d)))
	  (cons (/ n g) (/ d g))))

  (define (add-rat x y)
	(make-rat (+ (* (number x) (denom y))
				 (* (number y) (denom x)))
			  (* (denom x) (denom y))))

  (define (equ-rat? x y)
	(and (= (number x) (number y))
		 (= (denom x) (denom y))))

  (define (=zero-rat? x)
	(= (number x) 0))

  (define (sub-rat x y)
	(make-rat (- (* (number x) (denom y))
				 (* (number y) (denom x)))
			  (* (denom x) (denom y))))

  (define (mul-rat x y)
	(make-rat (* (number x) (denom y))
			  (* (denom x) (denom y))))

  (define (div-rat x y)
	(make-rat (* (number x) (denom y))
			  (* (denom x) (number y))))

  'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; complex number
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (install-complex-package)
  (put 'real-part 'complex
	   (lambda (x) real-part))

  (put 'imag-part 'complex
	   (lambda (x) imag-part))

  (put 'magnitude 'complex
	   (lambda (x) magnitude))

  (put 'angle 'complex
	   (lambda (x) angle))

  (put 'equ? '(complex complex)
	   (lambda (x y) (equ-complex? z1 z2)))

  (put '=zero? 'complex
	   (lambda (x) (=zero-complex? x)))

  (put 'add '(complex complex)
	   (lambda (x y) (tag (add-complex z1 z2))))

  (put 'sub '(complex complex)
	   (lambda (x y) (tag (sub-complex z1 z2))))

  (put 'mul '(complex complex)
	   (lambda (x y) (tag (mul-complex z1 z2))))

  (put 'div '(complex complex)
	   (lambda (x y) (tag (div-complex z1 z2))))

  (put 'make-from-real-imag 'complex 
	   (lambda (x y) (tag (make-from-real-imag x y))))

  (put 'make-from-mag-ang 'complex 
	   (lambda (x y) (tag (make-from-mag-ang x y))))

  (define (tag z) (attach-tag 'complex z))

  (define (add-complex z1 z2)
	(make-from-real-imag (+ (real-part z1) (real-part z2))
						 (+ (imag-part z1) (imag-part z2))))

  (define (sub-complex z1 z2)
	(make-from-real-imag (- (real-part z1) (real-part z2))
						 (- (imag-part z1) (imag-part z2))))

  ;; TODO
  (define (=zero-complex? z))

  (define (mul-complex z1 z2)
	(make-from-mag-ang (* (magnitude z1) (magnitude z2)) 
					   (+ (angle z1) (angle z2))))

  (define (div-complex z1 z2)
	(make-from-mag-ang (/ (magnitude z1) (magnitude z2))
						 (- (angle z1) (angle z2))))

  (define (equ-complex? z1 z2)
	(and (= (magnitude z1) (magnitude z2))
		 (= (angle z1) (angle z2))))

	;; or
	;; (define (equ-complex? z1 z2)
	;;   (and (= (real-part z1) (real-part z2))
	;;   (= (imag-part z1) (imag-part z2))))

  (define (make-from-real-imag x y)
	((get 'make-from-real-imag 'rectangular) x y))

  (define (make-from-mag-ang r a)
	((get 'make-from-mag-ang 'polar) r a))

  'done)
