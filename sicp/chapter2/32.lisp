(define (map proc items) 
  (if (null? items) 
	items 
	(cons (proc (car items)) (map proc (cdr items)))))
    
(define (subsets items) 
  (if (null? items) 
	(list items) 
	(let ((rest (subsets (cdr items)))) 
	  (display rest) 
	  (append  rest (map (lambda (x) (cons (car items) x)) rest)))))
