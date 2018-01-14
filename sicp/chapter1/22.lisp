(define (square x)
  (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        (else (find-divisor n (+ test-divisor 1)))))
      
      
(define (remainder a b)
  (if (< a b) 
      a
      (remainder (- a b) b)))
    
    
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
    
(define (report-prime elasped-time)
  (display " *** ")
  (display elasped-time))

(define (prime? n)
  (try-it 1 n))

(define (try-it start n)
  (cond ((or (> start n) (= start n)) #t)
         ((= (expmod start n n) start) (try-it (+ start 1) n))
         (else #f)))

      
(define (expmod base exp n)
  (cond ((= exp 0) 1)
        ((even? exp) 
                (remainder (square (expmod base (/ exp 2) n))
                           n))
        (else 
              (remainder (* base (expmod base (- exp 1) n)) 
                         n))))
