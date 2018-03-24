// omit 24 25 26 

(define (deep-reverse items)
  (cond ((null? items) items)
        ((not (pair? items)) items)
        (else (append (deep-reverse (cdr items))
                      (list (deep-reverse (car items)))))))
