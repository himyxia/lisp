(define (fib n)
  (cond ((= n 0) 0)
		((= n 1) 1)
		(else (+ (fib (- n 1))
				 (fib (- n 2))))))

(define (even? x)
  (= (remainder x 2) 0))

(define (sum-odd-squares tree)
  (cond ((null? tree) (list))
		((not (pair? tree) 
			  (if (odd? tree) tree 0)))
		(else (+ (sum-odd-squares (car tree))
				 (sum-odd-squares (cdr tree))))))

(define (even-fibs n)
  (define (next k)
	(if (> k n)
	  (list)
	  (let ((f (fib k))
			(if (even? f)
			  (cons f (next (+ k 1)))
			  (next (+ k 1)))))))
  (next 0))

(define (filter predicate sequence)
  (cond ((null? sequence) (list))
		((predicate (car sequence))
		 (cons (car sequence) 
			   (filter predicate (cdr sequence))))
		(else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
	initial
	(op (car sequence) 
		(accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
	(list)
	(cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) (list))
		((not (pair? tree)) (list tree))
		(else (append (car tree) (enumerate-tree (cdr tree))))))


(define (sum-odd-squares tree)
  (accumulate + 0 (map square (filter odd? (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons (list) (filter even? (map fib (enumerate-interval 0 n)))))
