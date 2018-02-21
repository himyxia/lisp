(define (accumulate term a next b null-value combiner)
    (if (> a b)
	        null-value
			      (combiner (term a) (accumulate term (next a) next b null-value combiner))))
    
(define (accumulate term a next b null-value combiner)
    (define (accumulate-iter result cur)
	    (if (> cur b)
		        result
				      (accumulate-iter (combiner cur result) (next cur))))
	   (accumulate-iter null-value a))
    
    
(define (product term a next b)
    (accumulate term a next b 1 *))

(define (sum term a next b)
    (accumulate term a next b 1 +))

