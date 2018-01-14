(define (square x)
  (* x x))

(define (remainder a b)
  (if (< a b)
	a
	(remainder (- a b) b)))

(define (smallest-divisor n)
  (find-divisor 2 n))

(define (next start)
  (+ (* 2 start) 1))

(define (find-divisor start n)
  (cond ((> (square start) n) n)
		((= (remainder n start) 0) start)
		(else (find-divisor (next start) n))))
