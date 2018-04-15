
(define (fold-left op initial seq)
   (define (iter result rest)
     (if (null? rest)
         result
         (iter (op result (car rest))
               (cdr rest)))) 
    (iter initial seq))


(accumlator / 1 (list 1 2 3 4))
(fold-left / 1 (list 1 2 3 4))

(accumlator list (list) (list 1 2 3 4))
(fold-left list (list) (list 1 2 3 4))



(define (reverse sequence)
  (accumlator (lambda (x y) (append y (list x))) (list) sequence))


(define (reverse sequence)
  (fold-left (lambda (x y) (append (list y) x)) (list) sequence))
  
(reverse (list 1 2 3 4))
