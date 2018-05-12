(define (remainder a b)
  (if (< a b)
      a
      (remainder (- a b) b)))
    

(define (square x)
  (* x x))

(define (even? x)
  (= (remainder x 2) 0))

(define (exp base n)
  (cond ((= n 0) 1)
        ((even? n) (square (exp base (/ n 2))))
        (else (* base (exp base (- n 1))))))


(define (cons a b)
  (* (exp 2 a)
     (exp 3 b)))
   
(define (car z)
  (define (count origin result)
    (if (= (remainder origin  2) 0)
      (count (/ origin 2) (+ result 1))
      result))
    (count z 0))
  
(define (cdr z)
  (define (count origin result)
    (if (= (remainder origin  3) 0)
      (count (/ origin 3) (+ result 1))
      result))
    (count z 0))
