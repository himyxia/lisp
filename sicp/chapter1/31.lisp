(define (product term a next b)
    (if (> a b) 
	        1
			      (* (term a) (product term (next a) next b))))

(define (product term a next b)
    (define (product-iter result cur)
	      (if (> cur b) 
			        result
					        (product-iter (* cur result) (next cur))))
	      
	  (product-iter 1 a))

(define (identify x) x)
(define (incr x) (+ x 1))

(product identify 1 incr 5)

(define (factorial n)
    (product identify 1 incr n))

(define (remainder a b)
    (if (< a b)
	        a
			      (remainder (- a b) b)))
    
(define (even? n)
    (= (remainder n 2) 0))

(define (pi-term n)
    (if (even? n)
	    (/ n (+ n 1))
	    (/ (+ n 1) n)))

(define (pi n)
    (* (product f 1 incr n) 2)


