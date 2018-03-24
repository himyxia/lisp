(define (map proc items)
  (if (null? items) 
	items
	(cons (proc (car items))
		  (map proc (cdr items)))))

(define (square x)
  (* x x))
    
(define (square-list items)
  (if (null? items)
      items
      (cons (square (car items)) (square-list (cdr items)))))
    
(define (square-list items)
  (map items square)
