;; fib(n) = fib(n-1) + fib(n-2)

(define (fib n)
  (cond ((= n 0) 0)
		((= n 1) 1)
		(else (+ (fib (- n 1))
				 (fib (- n 2))))))

(define (fib-iter a b max-count)
  (cond ((= max-count 0) a)
		(else (fib-iter (+ a b) a (- max-count 1)))))
