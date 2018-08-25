(define (sum-primes a b)
  (define (iter count accum)
    (cond ((> count b) accum)
          ((prime? count)
                   (iter (+ count 1) (+ count accum)))
          (else (iter (+ count 1) accum))))
  (iter a 0))


(define (expmod base exp n)
  (cond ((= exp 0) 1)
        ((even? exp)
                (remainder (square (expmod base (/ exp 2) n))
                           n))
        (else 
              (remainder (* base (expmod base (- exp 1) n))
                         n))))

(define (square x)
  (* x x))

(define (remainder a b)
  (if (< a b)
      a
      (remainder (- a b) b)))
    
    
(prime? 100)

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (accumulate op (op initial (car seq)) (cdr seq))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (sum-primes a b)
  (accumulate + 
              0
              (filter prime? (enumerate-interval a b))))
            
(sum-primes 1 100)




;; primitive procedure
;;cons-stream 
;;the-empty-stream
;;stream-null?
;; stream-car
;; stream-cdr

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s)) (stream-map proc (stream-cdr s)))))
    
    



(cons-stream)

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(stream-car (stream-cdr 
                        (stream-filter
                          prime?
                          (stream-enumerate-interval
                            1000 1000000))))
                          
(define (prime? n)
  (try-it 1 n))

(define (try-it start n)
  (cond ((or (> start n) (= start n)) #t)
        ((= (expmod start n n) start) (try-it (+ start 1) n))
        (else #f)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low (stream-enumerate-interval (+ low 1) high))))
      
    
(define (stream-filter pred stream)
  (cond ((stream-null?) the-empty-stream)
        ((pred (stream-car stream)
               (cons-stream (stream-car stream)
                            (stream-filter pred (stream-cdr stream)))))
        (else (stream-filter pred (stream-cdr stream)))))    
      
      
(define (memo-proc proc)
  (let ((already-run? false) (result false))
       (lambda ()
         (if (not already-run?) 
             (begin (set! result (proc)) 
                    (set! already-run? true) 
                    result)
             result)))))

;; (delay <proc>) => (memo-proc (lambda() <exp>))
;; (define (force <delayed-object>)
     ;; (<delay-object>))
  

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (begin
        (apply proc (map (lambda (s) (stream-car s)) argstreams))
        (apply stream-map
               (cons proc (map (lambda (s) (stream-cdr s)) argstreams))))))



;; 51

(define (display-line x)
  (newline)
  (display x))

(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)


;; 52
(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20)))
            
;; sum = 1
;; (cons-stream 1 (stream-map accum (2 (stream-enumerate-interval 3 20))))

(define y (stream-filter even? seq))

;;(cons-stream 3 (stream-map accum (3 (stream-enumerate-intnerval 4 20))))
;;(cons-stream 6 (stream-map accum (4 (stream-enumerate-intnerval 5 20))))
;; we got it 
;;(cons-stream 6 (delay (stream-filter even? (delay (stream-map accum (4 (delay (stream-enumerate-intnerval 5 20))))))))

(define z (stream-filter
            (lambda (x) (= (remainder x 5) 0)) seq))
;; (cons-stream 10 (delay (stream-filter (lambda (x) (= (remainder x 5) 0)) (delay (steram-map accum (delay (stream-enumerate-interval 5 20)))))))
          ;; so the x's value is 10
          

;; currently y is: (cons-stream 6 (delay (stream-filter even? (delay (stream-map accum (delay (stream-enumerate-intnerval 4 20))))) 
(stream-ref y 7)
;; cdr
;; (stream-filter even? (delay (stream-map accum (delay (stream-enumerate-intnerval 4 20)))) 
;;(car (cdr (cdr (cdr (cdr (cdr (cdr y)))))))
;; 128

;;6, 10, 28, 58, 70, 112, 128, 182, 202

;; current z is : (cons-stream 10 (delay (stream-filter (lambda (x) (= (remainder x 5) 0)) (delay (steram-map accum (delay (stream-enumerate-interval 5 20)))))))
(display-stream z)

(define (stream-for-each proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s)) 
                   (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))


(define (display-line x)
  (newline)
  (display x))

10







































(stream-car 
            (stream-cdr 
                        (stream-filter prime? (stream-enumerate-interval 1000 1000000))))


(define (stream-filter pred stream)
  (cond ((stream-null?) the-empty-stream)
        ((pred (stream-car stream))
               (cons-stream (stream-car stream)
                            (stream-filter pred (stream-cdr stream)))))
        (else (stream-filter pred (stream-cdr stream)))))    

;; evaluate (stream-enumerate-interval 1000 1000000) 
  ;; -> (cons 1000 (delay (stream-enumerate-interval 1001 1000000))
              
;; execute (stream-filter prime? (cons 1000 (delay (stream-enumerate-interval 1001 1000000)))
  ;; since 1000 is not prime
  ;; go to `(else (stream-filter pred (stream-cdr stream)))`
  ;; => (stream-filter prime? (stream-cdr stream)))`
  ;; => (stream-cdr stream) => (force (delay (stream-enumerate-interval 1001 1000000)))
  ;; => (cons 1001 (delay (stream-enumerate-interval 1002 1000000)))
  ;; => (stream-filter prime? (cons 1001 (delay (stream-enumerate-interval 1002 1000000))))
  
;; execute (stream-filter prime? (cons 1001 (delay (stream-enumerate-interval 1002 1000000)))
  ;; since 1001 is not prime
  ;; go to `(else (stream-filter pred (stream-cdr stream)))`
  ;; => (stream-filter prime? (stream-cdr stream)))`
  ;; => (stream-cdr stream) => (force (delay (stream-enumerate-interval 1002 1000000)))
  ;; => (cons 1002 (delay (stream-enumerate-interval 1003 1000000)))
  ;; => (stream-filter prime? (cons 1002 (delay (stream-enumerate-interval 1003 1000000))))
  
 ;;===========.... 
 
;; execute (stream-filter prime? (cons 1006 (delay (stream-enumerate-interval 1007 1000000)))
  ;; since 1006 is not prime
  ;; go to `(else (stream-filter pred (stream-cdr stream)))`
  ;; => (stream-filter prime? (stream-cdr stream)))`
  ;; => (stream-cdr stream) => (force (delay (stream-enumerate-interval 1007 1000000)))
  ;; => (cons 1007 (delay (stream-enumerate-interval 1008 1000000)))
  ;; => (stream-filter prime? (cons 1007 (delay (stream-enumerate-interval 1008 1000000))))
  
;; execute (stream-filter prime? (cons 1007 (delay (stream-enumerate-interval 1008 1000000)))
  ;; since 1007 is prime
  ;; go to `(cons-stream 1007 (stream-filter pred (stream-cdr stream)))`
  ;; => (cons 1007 (delay (stream-filter prime? (cons 1008 (delay (stream-enumerate-interval 1009 1000000)))
  ;; not => (cons 1007 (delay (stream-filter prime? (delay (stream-enumerate-interval 1008 1000000)))


;; from now on, stream-filter return   
  ;; => (cons 1007 (delay (stream-filter prime? (cons 1008 (delay (stream-enumerate-interval 1009 1000000)))
  ;; not => (cons 1007 (delay (stream-filter prime? (delay (stream-enumerate-interval 1008 1000000)))
                          
;; execute `stream-cdr`
  ;; => (stream-filter prime? (cons 1008 (delay (stream-enumerate-interval 1009 1000000))
  ;; since 1008 is not prime
  ;; go to `(else (stream-filter pred (stream-cdr stream)))`
  ;; => (stream-filter prime? (stream-cdr stream)))`
  ;; => (stream-cdr stream) => (force (delay (stream-enumerate-interval 1009 1000000)))
  ;; => (cons 1009 (delay (stream-enumerate-interval 1010 1000000)))
  ;; => (stream-filter prime? (cons 1009 (delay (stream-enumerate-interval 1010 1000000))))

;; execute (stream-filter prime? (cons 1009 (delay (stream-enumerate-interval 1010 1000000)))
  ;; since 1009 is prime
  ;; go to `(cons-stream 1009 (stream-filter pred (stream-cdr stream)))`
  ;; => (cons 1009 (delay (stream-filter prime? (cons 1010 (delay (stream-enumerate-interval 1011 1000000)))
  ;; not => (cons 1009 (delay (stream-filter prime? (delay (stream-enumerate-interval 1010 1000000)))


;; execute 'stream-car'
;; 1009

              
                        
                          



(cons 1000 (delay (stream-enumerate-interval 1001 1000000)))
(stream-filter prime? (cons 1000 (delay (stream-enumerate-interval 1001 1000000))))
(stream-filter prime? (stream-enumerate-interval 1001 1000000))
(stream-filter prime? (stream-enumerate-interval 1002 1000000))
;; not prime
(cons 1001 (delay (stream-enumerate-interval 1002 1000000)))
;; ...
(cons 1001 (delay (stream-enumerate-interval 1002 1000000)))
;; ...
(cons 10007 (delay (stream-filter prime? (stream-enumerate-interval 1008 1000000))))
(execute stream-cdr)
(stream-filter prime? (cons 1008 (delay (stream-enumerate-interval 1009 1000000))))
(stream-filter prime? (cons 1009 (delay (stream-enumerate-interval 1010 1000000))))
(cons 1009 (delay (stream-enumerate-interval 1010 1000000)))
