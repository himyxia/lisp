(define (sum-primes a b)
  (define (iter count accum)
    (cond ((> count b) accum)
          ((prime? count)
                   (iter (+ count 1) (+ count accum)))
          (else (iter (+ count 1) accum))))
        
  (iter a 0))

(define (sum-primes a b)
  (accumulate + 0 (filter prime? (enumerate-interval a b))))


(stream-car (stream-cdr
              (stream-filter
                prime? (stream-enumerate-interval 10000 1000000))))
              
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
        low
        (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
               (cons-stream (stream-car stream)
                            (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))      
  
  
;; (stream-enumerate-interval 1000 1000000)              
  ;; -> (cons-stream 10000 (stream-enumerate-interval 10001 1000000))
  ;; 10000 is not prime, -> (stream-cdr stream) -> 
  ;; start to evaluate '(stream-enumerate-interval 10001 1000000)'
  ;; which return (stream-cons 10001 (stream-enumerate-interval 10002 1000000))
  ;; until 10007
  
  ;; (cons-stream (stream-car stream) (stream-filter pred (stream-cdr stream)))
  ;; => (cons 10007 (delay (stream-filter prime? 
  ;;                                        (cons 10008 (delay (stream-enumerate-interval 10009 1000000))))))
  ;; .....
  
  ;; => (cons 10009 (delay (stream-filter prime? 
  ;;                                        (cons 10010 (delay (stream-enumerate-interval 10011 1000000))))))
  ;; => 10009
      
(cons-stream)
(stream-car (cons-stream x y)) = x
(stream-cdr (cons-stream x y)) = y

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream))

(the-empty-stream)

(stream-null?)

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
    
    
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
           
(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

















(delay <exp>)
  => (lambda () <exp>)
  
(define (force delayed-object)
  (delayed-object))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
       (lambda ()
         (if (not already-run?)
             (begin (set! result (proc))
                    (set! already-run? true)
                    result)
              result))))

(delay <exp>)
=> equals to  `(memo-proc (lambda () <exp>))`



;; infinitely stream

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

;;=> (cons n (delay (lambda () (cons-stream 2 (delay (intergers-staring-from 3))))))

(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

(stream-ref no-servens 100)

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))


(define fibs (fibgen 0 1))

(define (sieve stream)
  (cons-stream
    (stream-car stream)
    (sieve (stream-filter
             (lambda (x)
               (not (divisible? x (stream-car stream))))
             (stream-cdr stream)))))
           
           
(define primes (sieve (integers-starting-from 2)))

(stream-ref primes 50)
