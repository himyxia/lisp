(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess) (sqrt-improve guess x)) 
                             guesses)))
  guesses)


(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))
             
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))


(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2))
      (cons-stream (- s2 (/ (square (- s2 s1))
                            (+ s0 (* -2 s1) s2)))
                          (euler-transform (stream-cdr s))))))
                        
                        
(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs? (stream-cdr s) (stream-cdr t)))))
    
    
(define (stream-append s1 s2)
  (if (stream-null? s1) 
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))


(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)


;; differential
(define (solve f x0 dt)
  (define x (integral dx x0 dt))
  (define dx (stream-map f x))
  x)

(defeine (integral delayed-integrand inital-value dt)
         (define int
           (cons-stream initial-value
                        (let 
                             ((integrand (force delayed-integrand)))
                             (add-streams (scale-stream integrand dt)
                                          int))))
                                        int)
(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)






(define rand
  (let ((x random-init)
           (lambda () 
             (set! x (rand-update x))
             x))))


(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))


(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        ramdom-numbers)))

(define (map-ssuccessive-pairs f s)
  (cons-stream
    (f (stream-car s) (stream-car (stream-cdr s)))
    (map=-successive-pairs f (stream-cdr (stream-cdr s)))))
  
(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
      (/ passed (+ passed failed))
      (monte-carlo
        (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi
  (stream-map (lambda (p) (sqrt (/ 6 p)))
              (monte-carlo cesaro-stream 0 0)))
            
            

(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))
  
(define (stream-withdraw balance amount-stream)
  (cons-stream
    balance
    (stream-withdraw (- balance (stream-car amount-stream))
                     (stream-cdr amount-stream))))
  










