(define (square x)
  (* x x))

(define (prime? n)
  (fast-prime? n 100))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else #f)))
      

(define (miller-rabin-test n)
  (define (try-it a)
    (define (check-it x)
      (and (not (= x 0)) (= x 1)))
  
    (check-it (miller-rabin-expmod a (- n 1) n)))
    
  (try-it (+ 1 (random (- n 1)))))

(define (miller-rabin-expmod base expt m)
  (define (squaremod-with-check x)
    (define (check-nontrivial-sqrt1 x square)
      (if (and (= square 1) (not (= x 1) (not (= x (- m 1)))))
          0
          square))
        
    (check-nontrivial-sqrt1 x (remainder (square x) m)))
  
  
  (cond ((= expt 0) 1)
        ((even? expt)
                (squaremod-with-check
                  (miller-rabin-expmod base (/ expt 2) m)))
        (else 
              (remainder (* base (miller-rabin-expmod base (- expt 1) m)) m))))



;; (remainder (square a) m) = (remainder (square (remainder a m)) m) in the first place

;; now, the miller algorithm check
;; (remainder (square a) m) 
	;; = 0
	;; if (remainder a m) is not 1 or n-1, and (remainder (square (remainder a m)) m) = 1

	;; = (remainder (square (remainder a m)) m) 
	;; otherwise
