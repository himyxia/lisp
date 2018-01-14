;; 1.16
(define (expt base n)
  (if (= n 0) 1
      (* base (expt base (- n 1)))))
    
(define (expt-iter product base n)
  (if (= n 0) 
      product
      (expt-iter (* product base) base (- n 1))))
  

(define (remainder  a b)
  (if (< a b) 
      a
      (remainder (- a b) b)))


(define (square n)
  (* n n))

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt base n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt base (/ n 2))))
        (else (* base (fast-expt base (- n 1))))))
      


(define (fast-expt-iter a b n)
  (cond ((= n 0) a )
        ((even? n) 
                (fast-expt-iter (if (= a 1) b (square a))  b (/ n 2)))
        (else (fast-expt-iter (* a b) b (- n 1)))))


(define (fast-expt base n)
  (fast-expt-iter 1 base n))

;;1.17
