(define (sum term a next b)
    (define (iter a result)
	      (if (= a (next b)) 
			        result
					        (iter (next a) (+ result (term a)))))
	  (iter a 0))


(define (identify x) x)

(define (incr x) (+ x 1))

(sum identify 1 incr 10)
