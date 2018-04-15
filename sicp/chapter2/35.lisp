(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

      
(define (count-leaves x)
  (accumlator + 0 (map (lambda (x) 1) (enumrate-leaves x))))


(define (enumrate-leaves tree)
  (cond ((null? tree) (list))
        ((not (pair? tree)) (list tree))
        (else (append (enumrate-leaves (car tree))
                      (enumrate-leaves (cdr tree))))))
