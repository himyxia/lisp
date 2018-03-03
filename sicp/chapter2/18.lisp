(define (reverse items)
  (cond ((null? items) (list))
        ((= (length items) 1) items)
        (else (append (reverse (cdr items)) (list (car items))))))
