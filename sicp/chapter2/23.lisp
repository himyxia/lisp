(define (for-each items proc)
  (define (iter things result)
    (if (null? things)
        result
        (iter (cdr things) (proc (car things)))))
  (iter items ())) 
