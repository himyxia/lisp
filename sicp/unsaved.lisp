(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) 
	(* 3 (- 6 2) (- 2 7)))

(define (square x) (* x x))

(define (sum-square a b) (+ (square a) (square b)))

(define (larger-sum-square a b c) 
  (cond ((and (< a b) (< a c)) (sum-square b c)) 
		((< b c) (sum-square a c)) 
		(else (sum-square a b))))


(define (smaller a b) 
  (if (< a b) 
	a 
	b))

(larger-sum-square 1 1 3)

(define (a-plus-abs-b a b)
((if (> b 0) + -) a b))

(a-plus-abs-b 1 4)

(define (abs x)
(if (> x 0)
x
(- x)))

(define (improve guess x)
(/ (+ (/ x guess) guess) 2))

(define (good-enough? guess x)
(< (abs (- (square guess) x)) 0.001))


(define (new-if predicate then-clause else-clause)
(cond (predicate then-clause)
(else else-clause)))

(define (sqrt-iter guess x)
(if (good-enough? guess x)
guess
(sqrt-iter (improve guess x) x)))


(define (sqrt x)
(sqrt-iter 1 x))

(sqrt 3)

(define (improve-cube-root guess x)
(/ (+ (/ x (square guess))
(* 2 guess))
3))

(define (cube-root-iter guess x)
(if (good-enough? guess x)
guess
(cube-root-iter (improve-cube-root guess x) x)))

(define (cube-root x)
(cube-root-iter 1 x))


(define (average a b)
(/ (+ a b) 2))


(define (sqrt x)
(define (good-enough? guess)
(< (abs (- (square guess) x)) 0.001))

(define (improve guess)
(average (/ x guess) guess))

(define (sqrt-iter guess)
(if (good-enough? guess)
guess
(sqrt-iter (improve guess x) x)))
(sqrt-iter 1.0 x))



(sqrt 3)



(define (factorial x) 
  (if (= x 1) 
	1 
	(* (factorial (- x 1)) x)))


(define (factorial n) 
  (define (factorial-iter acc counter total-counter) 
	(if (> counter total-counter) 
	  acc 
	  (factorial-iter 
		(* acc counter) 
		(+ counter 1) 
		total-counter)))

(factorial-iter 1 1 n))


(define (A x y)
(cond ((= y 0) 0)
((= x 0) (* 2 y))
((= y 1) 2)
(else (A (- x 1)
				(A x (- y 1))))))

;; (A 0 n) computes 2*n
;; (A 1 n) computes 2^n
;; (A 2 n) computes 2^(+ 1 n)

(A 2 3)         

;; fibonacci number

(define (fib n) 
  (cond ((= n 0) 0) 
		((= n 1) 1) 
		(else (+ (fib (- n 1)) (fib (- n 2))))))

(define (fib n) 
  (define (fib-iter a b count max-count) 
	(cond ((= max-count 1) a) 
		  ((= max-count 2) b) 
		  ((= count max-count) (+ a b)) 
		  (else (fib-iter (+ a b) a (+ count 1) max-count)))) 
  (fib-iter 1 1 3 n))

(define (fib n) 
  (define (fib-iter a b count max-count) 
	(cond ((= max-count 0) a) 
		  ((= max-count 1) b) 
		  ((= count max-count) (+ a b)) 
		  (else (fib-iter (+ a b) a (+ count 1) max-count)))) 
  (fib-iter 0 1 1 n))

(define (fib n) 
  (define (fib-iter a b count) 
	(cond ((= count 0) a) 
		  (else (fib-iter (+ a b) a (- count 1))))) 
  (fib-iter 0 1 n))

(fib 7)

(define (count-change amount)
(cc 5 amount))

(define (cc type-of-coins amount) 
  (cond ((= amount 0) 1) 
		((or (< amount 0) (= type-of-coins 0)) 0) 
		(else (+ (cc (- type-of-coins 1) amount) 
				 (cc type-of-coins (- amount (first-denomination type-of-coins)))))))

(define (first-denomination type-of-coins) 
  (cond ((= type-of-coins 1) 1) 
		((= type-of-coins 2) 5) 
		((= type-of-coins 3) 10) 
		((= type-of-coins 4) 25) 
		((= type-of-coins 5) 50)))    

(count-change 100)


(define (f n)
(if (< n 3) 
n
(+ (f (- n 1))
(* 2 (f (- n 2)))
(* 3 (f (- n 3))))))


(f 100)
