// recursive way
(define (cont-frac n d k)
  (define (start-from i)
	(if (> i k)
	  0
	  (/ (n i) (+ (d i) (start-from (+ i 1))))))
  (start-from 1))

// interactive

(define (cont-frac n d k)
  (define (iter i result)
	(if (= i 0) 
	  result
	  (iter (- i 1) ((n i) (+ (d i) result)))))
  (iter k 0.0))

(cont-frac (lambda (i) 
			 (if (= i 1)
			   x
			   (- (square x))))
		   (lambda (i)
			 (- (* i 2) 1))
		   100)
