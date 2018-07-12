(define make-rat cons)

(define (number x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (number x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))

(define (add-rat x y)
  (make-rat (+ (* (number x) (denom y)) 
                  (* (denom x) (number y)))
                (* (denom x) (denom y))))
              
(define (sub-rat x y)
  (make-rat (- (* (number x) (denom y))
               (* (number y) (denom x)))
             (* (denom x) (denom y))))
           
(define (mul-rat x y)
  (make-rat (* (number x) (number y))
            (* (denom x) (denom y))))
          
(define (div-rat x y)
  (make-rat (* (number x) (denom y))
            (* (denom x) (number y))))
          
(define (equal-rat? x y)
  (= (* (number x) (denom y))
     (* (number y) (denom x))))
   
   
(define (make-rat x y)
  (if (> (* x y) 0)
      (if (< x 0) 
          (cons (- x) (- y))
          (cons x y))
      (if (> x 0) 
          (cons (- x) (- y))
          (cons x y))))
        
        
(define (remainder a b)
  (if (< a b)
      a
      (remainder (- a b) b)))

(define (gcd a b)
  (if (= b 0) 
      a
      (gcd b (remainder a b))))
