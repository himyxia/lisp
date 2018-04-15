(define first (list 1 3 (list 5 7) 9))	
(car (cdr (car (cdr (cdr first)))))

(define second (list (list 7)))
(car (car second))

(define third (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr third))))))))))))


(define (even? x)
  (= (remainder x 2) 0))

(define (odd? x)
  (not (even? x)))

(define (remainder x y)
  (if (< x y) 
      x
      (remainder (- x y) y)))
  
(define (square x)
  (* x x))



(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))  
               
(define (fib-iter n)
  (define (iter a b k)
    (if (= k n) 
        b
        (iter b (+ a b) (+ k 1))))
  (iter 0 1 1))
  
  (fib-iter 7)
  
  
(define (even-fib n)
  (define (iter k prod)
    (cond ((or (= k n) (> k n)) prod)
          ((even? (fib k)) (iter (+ k 1) (append prod (list (fib k)))))
          (else (iter (+ k 1) prod))))
        
  (iter 0 (list)))
        

(define (even-fibs n)
  (define (next k)
    (if (> k n) 
        (list)
        (let ((f (fib k)))
             (if (even? f) (cons f (next (+ k 1)))
                 (next (+ k 1))))))
  (next 0))

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) (if (odd? tree) tree 0))
        (else (+ (square (sum-odd-squares (car tree)))
                 (square (sum-odd-squares (cdr tree)))))))
        


(map square (list 1 2 3 4))

(define (enumrate-interval low high)
  (if (> low high)
      (list)
      (cons low (enumrate-interval (+ low 1) high))))

(enumrate-interval 1 5)

(define (filter predicate l)
  (cond ((null? l) (list))
        ((predicate (car l)) (cons (car l) (filter predicate (cdr l))))
        (else (filter predicate (cdr l)))))

(define (even-fibs n)
  (filter even? (map fib (enumrate-interval 0 n))))

(even-fibs 11)

(define (enumrate-leaves tree)
  (cond ((null? tree) (list))
        ((not (pair? tree)) (list tree))
        (else (append (enumrate-leaves (car tree))
                      (enumrate-leaves (cdr tree))))))

(enumrate-leaves first)

(define (accumlator op init seq)
  (if (null? seq) 
      init
      (op (car seq) (accumlator op init (cdr seq)))))

(define (sum-odd-squares tree)
  (accumlator + 0 (map square (filter odd? (enumrate-leaves tree)))))

(sum-odd-squares first)

(define (list-fib-squares n)
  (accumlator cons (list) (map square (map fib (enumrate-interval 0 n)))))

(list-fib-squares 11)

(define (product-of-squares-of-odd-elements seq)
  (accumlator * 1 (map square (filter odd? seq))))

(product-of-squares-of-odd-elements (list 1 2 3 4 5 6))

(define (salary-of-highest-paid-programmer records)
  (accumlator max 0 (map salary (filter programmer? records))))

(define (map p sequence)
  (accumlator (lambda (x y) (cons (p x) y)) (list) sequence))

(map square (list 1 2 3 4 5))

(define (append seq1 seq2)
  (accumlator cons seq2 seq1))

 (append (list 1 2 3 4) (list 5 6 7 8)) 
 
(define (length seq)
  (accumlator (lambda (x y) (+ 1 y)) 0 seq))
  
(length (list 1 2 3 4))

(define (horner-eval x coefficient-seq)
  (accumlator (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
               0
               coefficient-seq))
  
(horner-eval 2 (list 1 3 0 5 0 1)) 

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

      
(define (count-leaves x)
  (accumlator + 0 (map (lambda (x) 1) (enumrate-leaves x))))
  
  
(count-leaves first)
  
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      (list)
      (cons (accumlator op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
  
