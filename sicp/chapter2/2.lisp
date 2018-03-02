(define make-point cons)
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define make-seg cons)
(define (start-seg s) (car s))
(define (end-seg s) (cdr s))


(define (midpoint-seg seg)
  (let ((start (start-seg seg))
       (end (end-seg seg)))
  (make-point (/ (+ (x-point start)
                 (x-point end)) 2)
              (/ (+ (y-point start)
                    (y-point end)) 2))))
                  
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
