(define (double x)
  (* 2 x))

(define (halve x)
  (/ x 2))

(define (remainder a b)
  (if (< a b) 
	a
	(remainder (- a b) b)))

(define (even? x)
  (= (remainder x 2) 0))

(define (fast-mul a b)
  (cond ((= a 1) b)
		((even? a) (double (fast-mul (halve a) b)))
		(else (+ a(fast-mul (- a 1) b)))))
