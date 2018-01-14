;; 561, 1105, 1729, 2465, 2821, 6601

(define (prime? n)
  (full-fermat-test? n))

(define (full-fermat-test? n)
  (define (iter a)
	(cond ((= a n) #t)
		  ((= (expmod a n n) (iter (+ a 1))))
		  (else #f)))
  (iter 2))

(define (square x)
  (* x x))

(define (remainder a b)
  (if (< a b) 
	a
	(remainder (- a b) b)))

(define (even? a)
  (= (remainder a 2) 0))

(define (expmod base expt m)
  (cond ((= expt 0) 1)
		((even? expt) 
		 (remainder (square (expmod base (/ expt 2) m)) m))
		(else (remainder (* base (expmod base (- expt 1) m)) m))))

(define (fermat-test a n)
  (= (expmod a n n) a))
