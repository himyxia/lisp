(define (square x) (* x x))

(define (map proc items)
  (if (null? items) 
	items
	(cons (proc (car items))
		  (map proc (cdr items)))))
     
(define (square-tree items)
  (cond ((null? items) items)
        ((not (pair? items)) (square items))
        (else (cons (square-tree (car items))
                    (square-tree (cdr items))))))
     
(define (square-tree items)
  (map (lambda (subtree)
         (if (pair? subtree) 
             (square-tree subtree)
              (square subtree)))
       items))    
