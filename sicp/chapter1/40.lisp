(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
    dx)))
  
(define (cube x) (* x x x))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
  
(define (newtons-method g guess)
  (fixed-point (newton-tranform g) guess)
  
(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))


(define (fixed-point-of-transform g tranform guess)
  (fixed-point (tranform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-tranform
                            1.0)

(define (cubic a b c) 
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(newtons-method (cubic a b c) 1)
