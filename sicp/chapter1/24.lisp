(define (prime? n)
  (fast-prime n 100))

(define (fast-prime n times)
  (cond ((= times 0) n)
		((fermat-test n) (fast-prim n (- times 1)))
		(else false)))

(define (fermat-test n)
  (define (try-it a)
	(= (expmod a n n) a))
	  
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
		((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
		(else (remainder (* base (expmod base (- exp 1) m)) m))))
