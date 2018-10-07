(set! balance (+ balance 10))

a b c 
x y z

procedure:
  access
  assignment
  
  
// serializer

(paraller-execute)

(define x 10)
(paraller-execute (lambda () (set! x (* x x)))
                  (lanbda () (set! x (+ x 1))))
                
                
(define x 10)
(define s (make-serializer))

(paraller-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (+ x 1)))))


(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
      
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  
  (let ((protected (make-serializer)))
       (define (dispatch m)
         (cond ((eq? m 'withdraw) (protected withdraw))
               ((eq? m 'deposit) (protected deposit))
               ((eq? m 'balance) balance)
               (else (error "Unknown request")))))))
       
(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
       ((account1 'withdraw) difference)
       ((account2 'deposit) difference)))
     
     



       
(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balace (- balance amount))
               balance)
             "Insufficient funds"))
           
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  
  (let ((balance-serializer (make-serializer)))
       (define (dispatch m)
         (cond ((eq? m 'withdraw) withdraw)
               ((eq? m 'deposit) deposit)
               ((eq? m 'balance) balance)
               ((eq? m 'serializer) balance=serializer)
               (else (error "Unknown request"))))
             
        dispatch))       
      
(define (deposit account amount)
  (let ((s (account 'serializer))
           (d (account 'deposit)))
       ((s d) amount)))
     
     
(define (serialized-exchange account1 account2)
  (let ((serializer-1 (account1 'serializer))
                      (serializer-2 (account2 'serializer)))
      ((serializer1 (serializer2 exchange)) account1 account2)))
    
;;serializer -> mutex

(define (make-serializer)
  (let ((mutex (make-mutex)))
       (lambda (p)
         (define (serialized-p . args)
           (mutex 'acquire)
           (let ((val (apply p args)))
                (mutex 'release)
                val))
        serialized-p)))
      
      
(define (make-mutex)
  (let ((cell (list false)))
       (define (the-mutex m)
         (cond ((eq? m 'acquire)
                     (if (test-and-set! cell)
                         (the-mutex 'acquire))); retry
               ((eq? m 'release) (clear! cell))))
             
        the-mutex))
      
      
(define (clear! cell)
  (set-car! cell false))

;; must be performed atomatically
(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))
       
peter: want to exchange (a1, a2)
paul: want to exchange (a2, a1)
