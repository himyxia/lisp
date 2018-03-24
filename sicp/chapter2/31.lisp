(define (map proc items)
  (if (null? items) 
	items
	(cons (proc (car items))
		  (map proc (cdr items)))))

(define (tree-map proc items)
  (map (lambda (subtree)
         (if (pair? subtree) 
             (tree-map proc subtree)
             (proc subtree)))
       items))  
