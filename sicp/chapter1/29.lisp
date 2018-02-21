(define (sum f a b term next)
  (if (> a b)
      0
      (+ (term a) (sum f (next a) b term next))))
    

      
(define (remainder a b)
  (if (< a b) 
      a
      (remainder (- a b) b)))


(define (even? a)
  (= (remainder a 2) 0))




(define (cube x)
  (* x x x))

(define (integral f a b n)
  
  (define (prefix k)
  (cond ((or (= k 0) (= k n)) 1)
        ((even? k) 4)
        (else 2)))
      
  (define cal-h
  (/ (- b a) n))
  
  (define (cal-y k)
  (f (+ a (* k cal-h))))

  (define (term k)
    (* (prefix k) (cal-y k)))
  
  (define (incr x)
    (+ x 1))
  
  (* (/ cal-h 3) (sum f 0 n term incr)))
