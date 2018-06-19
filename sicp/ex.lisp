(cadr arg) = (car (cdr arg))

(define pi 3.1415)

(define radius 10)

(define circumference (* 2 pi radius))

(define (fib n)
	(cond ((= n 0) 0)
		  ((= n 1) 1)
		  (else (+ (fib (- n 1)) 
		  	       (fib (- n 2))))))

(define (reminder a b)
	(if (< a b) 
		a 
		(reminder (- a b) b)))

(define (gcd a b)
	(if (= b 0)
		a
		(gcd (b (reminder a b)))))


;; ratio number
(define (make-ratio n d)
	(let ((g ((if (< d 0) - +) (abs (gcd n d)))))
		 (cons (/ n g) (/ d g))))

(define (number x) (car x))

(define (denom x) (cdr x))

(define (add-ratio x y)
	(make-ratio (+ (* (number x) (denom y))
				   (* (number y) (denom x)))
				(* (denom x) (denom y))))

(define (print-ratio x)
	(newline)
	(display (number x))
	(display "/")
	(display (denom x)))


(define (sub-ratio x y)
	(make-ratio (- (* (number x) (denom y))
				   (* (number y) (denom x)))
				(* (denom x) (denom y))))

(define (mul-ratio x y)
	(make-ratio (* (number x) (number y))
				(* (denom x) (denom y))))

(define (div-ratio x y)
	(make-ratio (* (number x) (denom y))
			    (* (denom x) (number y))))

(define (equal-ratio? x y)
	(= (* (number x) (denom y))
	   (* (number y) (denom x))))




;;(define one-half (make-rat 1 2))

;;(define one-third (make-rat 1 3))

;;(print-rat (add-rat one-half one-third))

;;(print-rat (add-rat one-third one-third))


;; point
(define (make-point x y) (cons x y))

(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (print-point p) 
	(newline)
	(display "(")
	(display (x-point p))
	(display " ,")
	(display (y-point p))
	(display ")"))

;; sequence
(define (make-seq start-point end-point)  (cons start-point end-point)) 
(define (start-seq s) (car s))
(define (end-seq s) (cdr s))

(define (midpoint-seq s)
	(let ((a (start-seq s))
		  (b (end-seq s)))
		  (make-seq (average (x-point a) (x-point b))
					(average (y-point a) (y-point b)))))

(define xx (make-point 5 8))

(print-point xx)

(midpoint-seq (make-seq (make-point 1 1) 
						(make-point 4 6)))

(define (len seq)
	(len ((a (end-seq seq))
		  (b (start-seq seq)))
		(sqrt (+ (square (- (x-point a) 
							(x-point b)))
				 (square (- (y-point a)
							(y-point b)))))))

;; rect
(define (make-rect v h) (cons v h))

(define (vert-rect rect) (car rect))

(define (horn-rect rect) (cdr rect))

(define (perimeter rect) 
	(* 2 
	   (+ (len (vert-rect rect)
		  (len (horn-rect rect)))))

(define (area rect) 
	(* (len (vert-rect rect)) (len (horn-rect rect))))



(define (cons x y)
	(lambda (m) (m x y)))

(define (car z)
	(z (lambda (p q) p)))

(define (car z)
	(z (lambda (p q) q)))

(define (even? a)
	(= (reminder a 2) 0 ))

(define (square x)
	(* x x))


;; exponential
(define (exp base n)
	(define (iter product term)
		(if (= term n)
			product
			(iter (* base product) (+ term 1))))
	(iter 1 0))

(define (exp a b)
	(cond ((= b 0) 1)
		  ((even? b) (square (exp a (/ b 2))))
		  (else (* a (exp a (- b 1))))))

(define (cons x y)
	(* (exp 2 x) (exp 3 y)))

;; ??
(define (reminder-division a b)
	(define (try n)
		(if (= (reminder a (exp b n)) 0)
			(try (+ n 1))
			(- n 1)))
	(try 1))

(define (car z)
	(reminder-division z 2))

(define (cdr z)
	(reminder-division z 3))

(define z (cons 5 7))

;; ex1.15
(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
	  angle
	  (p (sine (/ angle 3.0)))))

(sine 12.15)

(p (sine 4.5))

(p (p (sine 1.5)))

(p (p (p (sine 0.5))))

(p (p (p 0.5)))

(p (p 1.05))

(p (p 1.05))

(sine a)


;; how much times it take from 0.1 to get 12.15 by multiply 3 ?
	(ceiling(/ (log (/ 12.15 0.1)) (log 3)))

	log(/ 121.5 3)

;; ex1.16
(define (expt b n)
	(define (expt-iter a base count)
		(cond ((= count 0) a)
			  ((even count) (expt-iter a (* base base) (/ count 2)))
			  (else (expt-iter (* a base) base (- count 1)))
			  ))

	(expt-iter 1 b n))

;; 1.17
;; design multiplication procedure using logarithmic number of steps
;; double halve 

(define (double x) (+ x x))

(define (halve x) (/ x 2))

(define (* a b)
	(cond ((= b 0) 0) 
		  ((even b) (* (double base) (halve count)) 
		  (else (* base (* base (- count 1)))))))


;;1.18
	// iteractive process
	// add
	// double
	// halving
	// logarithmic

(define (* a b)
)

;;1.19:
	1. (define (+ a b)
		(if (= a 0)
			b
			(inc (+ (dec a) b))))

	this procedure will generate processes:
		-------------------
		(+ 4 5)
			↓
		(inc (+ (dec 4) 5))
		(inc (+ 3 5))
			↓
		(inc (inc (+ (dec 3) 5)))
		(inc (inc (+ 2 5)))
			↓
		(inc (inc (inc (+ 1 5))))
			↓
		(inc (inc (inc (inc (+ 0 5)))))
		(inc (inc (inc (inc 5))))
			↓
		(inc (inc (inc 6)))
			↓
		(inc (inc 7))
			↓
		(inc 8)
			↓
			9
		-------------------

	2. (define (+ a b)
		(if (= a 0)
			b
			(+ (dec a) (inc b))))

	this procedure will generate processes:
		-------------------
		(+ 4 5)
			↓
		(+ 3 6)
			↓
		(+ 2 7)
			↓
		(+ 1 8)
			↓
		(+ 0 9)
			↓
			9
		-------------------
	

	3. (define (A x y)
		(cond ((= y 0) 0
			  ((= x 0) (* 2 y))
			  ((= y 1) 2)
			  (else (A (x-1)
			  		   (A x (- y 1)))))))

		(A 1 10) :
			(A 0 (A 1 9))
			(A 0 (A 0 (A 1 8))
			(A 0 (A 0 (A 0 (A 1 7))
			...
			(A 0 (A 0 (A 0 (A 0(A 0(A 0(A 0(A 0(A 0(A 1 1))))))))))

			(A 0 (A 0 (A 0 (A 0(A 0(A 0(A 0(A 0(A 0 2)))))))))

			(A 0 (A 0 (A 0 (A 0(A 0(A 0(A 0(A 0 4))))))))

			(A 0 (A 0 (A 0 (A 0(A 0(A 0(A 0 8)))))))
			(A 0 (A 0 (A 0 (A 0(A 0(A 0 16))))))
			(A 0 (A 0 (A 0 (A 0(A 0 32)))))
			(A 0 (A 0 (A 0 (A 0 64))))
			(A 0 (A 0 (A 0 128)))
			(A 0 (A 0 256))
			(A 0 512)
			1024

		(A 2 5):
			(A 1 (A 2 4))
			(A 1 (A 1 (A 2 3))
			(A 1 (A 1 (A 1 (A 2 2))
			(A 1 (A 1 (A 1 (A 1 (A 2 1))
			(A 1 (A 1 (A 1 (A 1 2))
			(A 1 (A 1 (A 1 (A 0 (A 1 1))
			(A 1 (A 1 (A 1 (A 0 2)
			(A 1 (A 1 (A 1 4) 
			(A 1 (A 1 (A 0 (A 1 3) 
			(A 1 (A 1 (A 0 (A 0 (A 1 2) 
			(A 1 (A 1 (A 0 (A 0 (A 0 (A 1 1) 
			(A 1 (A 1 (A 0 (A 0 (A 0 2) 
			(A 1 (A 1 (A 0 (A 0 4)
			(A 1 (A 1 (A 0 8) 
			(A 1 (A 1 16) 
			(A 1 (A 1 16) 
			...

		(define (f n) (A 0 n))
			(f n) = 2*n
		(define (g n) (A 1 n))
			(g n) = 2^n
		(define (h n) (A 2 n))
			(h n) = 2^n
		(define (k n) (A 5 n))
			(k n) = 5*n*n
exe1.20
	(define (reminder a b)
		(cond ((< a b) a)
			  ((reminder (- a b) b))))

	(define (gcd a b)
		(if (= b 0)
			a
			(gcd b (reminder a b))))
	

	// normal order
		-----------------------
		(gcd 206 40)

		(gcd 40 (reminder 206 40))

		(gcd (reminder 206 40) (reminder 40 (reminder 206 40)))

		(gcd (reminder 40 (reminder 206 40)) (reminder (reminder 206 40) (reminder 40 (reminder 206 40))))

		(gcd (reminder 40 (reminder 206 40)) (reminder (reminder 206 40) (reminder 40 (reminder 206 40))))

		(gcd (reminder (reminder 206 40) (reminder 40 (reminder 206 40))) (reminder (reminder 40 (reminder 206 40)) (reminder (reminder 206 40) (reminder 40 (reminder 206 40)))))

		(gcd (reminder (reminder 206 40) (reminder 40 (reminder 206 40))))



		(gcd 6 (reminder 40 6))

		(reminder 40 6)

		(reminder 34 6)

		(reminder 28 6)

		(reminder 22 6)

		(reminder 16 6)
		
		(reminder 10 6)

		(reminder 4 6)
			4

		(gcd 6 4)

		(gcd 4 (reminder 6 4))

		(reminder 6 4)

		(reminder 2 4)

		2

		(gcd 4 2)

		(gcd 2 (reminder 4 2))

		(reminder 4 2)
		
		(reminder 2 2)

		(reminder 0 2)
		0

		(gcd 2 0)
		2
		-----------------------
	// application order
		-----------------------
		(gcd 206 40)
		(gcd 40 (reminder 206 40))
			(reminder 166 40)
			(reminder 126 40)
			(reminder 86 40)
			(reminder 46 40)
			(reminder 6 40)
				6
		-----------------------


(define (smallest-divisor n)
	(find-divisor n 2))

(define (divides? a b)
	(= (reminder b a) 0))

(define (find-divisor n test-divisor)
	(cond ((> (square test-divisor) n) n)
		  ((divides? test-divisor n) test-divisor)
		  (else (find-divisor n (+ test-divisor 1)))))

(define (prime? n)
	(= n (smallest-divisor n)))

(define (expmod base exp m)
	(cond ((= exp 0) 1)
		  ((even? exp) 
		  	(reminder (square (expmod base (/ exp 2) m)) m))
			// eg: 7 4 -> 3
			// 		49 4 -> 1
					// equals to
			// 		3*3 4 -> 1
		  (else
		  	(reminder (* base (expmod base (- exp 1)  m))
			m))))

(define (fermat-test n)
	(define (try-it a)
		(= (expmod a n n) a))
	(try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
	(cond ((= times 0) true)
		((fermat-test n) (fast-prime? n (- times 1)))
		(else false)))

(define (mod a b)
  (if (< a b) 
	a
	(mod (- a b) b)))

ex1.21 
	(define (reminder a b)
	  (if (< a b)
		  a
		  (reminder (- a b) b)))

	(define (next test-divisor)
		(if (= test-divisor 2)
			3
			(+ test-divisor 2))))

	(define (find-divisor n test-divisor)
	  (cond ((> (square test-divisor) n) n)
			((= (reminder n test-divisor) 0) test-divisor)
			(else (find-divisor n (next test-divisor)))))
		  
	(define (smallest-divisor n)
	  (find-divisor n 2))


	(smallest-divisor 19999)

;; 1.2.2

100: 50, 25, 10, 5, 1

(define (cc amount kind-of-coins)
  (cond ((= amount 0) 1) 
        ((or (< amount 0) (= kind-of-coins 0)) 0)
    		(else (+ (cc (- amount (first-denomination kind-of-coins)) kind-of-coins)
          		   (cc amount (- kind-of-coins 1))))))

(define (count-change amount) 
	(cc amount 5))

(count-change 100)

		
-> ex1.28

* 5 (fast-expt base 100)

* 5 (square (fast-expt base 50)

* 5 (square square(fast-expt base 25)

* 5 (square square(* 5 (fast-expt base 24)

* 5 (square square(* 5 (square (fast-expt base 12)

* 5 (square square(* 5 (square (square (fast-expt base 6)

* 5 (square square(* 5 (square (square (square (fast-expt base 3)

* 5 (square square(* 5 (square (square (square (* 5 square(fast-expt base 2)

* 5 (square square(* 5 (square (square (square (* 5 square(square 5)


square 5

square 25

square 625*5

reminder (* 5 (expmod 5 100 101) 101)

reminder (* 5 (reminder (square (expmod 5 50 101)) 101)

reminder (* 5 (reminder (square (reminder (square (expmod 5 25 101)) 101) 101)))

(define (sum-of-squares x y)
  ( + (square x ) (square y)))

(define (f a)
  (sum-of-squares (+ a 1 ) (* a 2)))

(define (average x y)
  (/ (+ x y) 2))

     
(sum-of-squares 2 3)

(/ (+ 5 4 
      (- 2 
         (- 3 
            (+ 6 
               (/ 4 5)))))
   (* 3 
      (- 6 2) 
      (- 2 7)))

(define (smaller x y)
  (if (< x y)
      x
      y))
    
(define (smallest x y z)
  (smaller x (smaller y z)))


(define (sum-of-larger-square x y z)
  (- (+ (square x) (square y) (square z)) (square (smaller x y z))))

(sum-of-larger-square 2 3 3)

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b 5 -8)

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
      
(new-if (> 2 0) 5 1)

(define (abs x)
  ((if (> x 0) + -) x))

(define (abs a)
	((if (< 0) - +) a))

;; computer sqrt root
;;---------------------------------------------
(define (sqrt-good-enought guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (imporve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (sqrt-good-enought guess x)
      guess
      (sqrt-iter (imporve guess x) 
                 x)))

(define (sqrt x)
	(sqrt-iter 1.0 x))
;;---------------------------------------------

(define (avarage-damp f)
	(lambda (x) (average x (f x))))

(define (sqrt x)
	(fixed-point (average-damp (lambda (y) (/ x y)))
		1.0))


 (define (cube-good-enough guess x) 
   (< (abs (- (cube guess) x)) 0.001)) 

 (define (improve-cube-guess guess x) 
   (/ (+ (/ x (square guess)) (* 2 guess)) 3)) 

(define (cube-root-iter guess x)
  (cond (cube-good-enough guess x)
        guess
        (cube-root-iter (improve-cube-guess guess x) 
                        x)))
      
(define (cube-root x)
  (cube-root-iter 1.0 x))

(cube-root 88)

(define (sum-integers a b)
  (if (> a b) 
	0
	(+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
	0
	(+ (cube a) (sum-cubes (+ a 1) b)))
 
(define (pi-sum a b)
  (if (> a b)
	0
	(( + (/ 1 (* a (+ a 2))) pi-sum (+ a 4) b))))

(define (<func> a b)
  (if (< a b)
	0
	(+ <term> (<func> <next> b))))



(define (sum-integers a b)
  (define (identify x) x)
  (define (next a) (+ a 1))
  (sum identify a next b))

(define (sum-cubes a b)
  (define (next a) (+ a 1))
  (sum cube a next b))

(define (pi-sum a b)
  (define (pi-term x) (/ 1 (* x (+ x 2))))
  (define (pi-next x) (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b)
  (define (integral-next x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) integral-next b) dx))

(define (incr x)
  (+ x 1))

(define (reminder a b)
  (if (< a b) 
	a
	(reminder (- a b) b)))


(define (simpson f a b n)
  (define h (/ (- b a) n))

  (define (y k) (f (+ a (* k h))))

  (define (simpson-term k) 
	(if (or (= k 0) (= k n))
		(y k)
		(if (even? k) 
		  (* 2 (y k))
		  (* 4 (y k)))))

  (define (coeff k) (if (even? k) 2 4))
  (define (part-term k) (* (coeff k) (y k))
  (define part-value (sum part-term 1 incr (- n 1)))
  (* (/ h 3) (+ (y 0) part-value (y n)))


(define (sum term a next b)
  (if (< a b)
	0
	(+ term (sum term (next a) next b))))

(define (sum term a next b)
  (define (iter result a)
	  (if (< a b)
		result
	  (iter (+ accumulator (term a)) (next a))))

	(iter 0 a)))

(define (sum term a next b)
  (define (iter a result)
	(if (> a b) 
	  result
	  (iter (+ (next a) (+ result (term a)))))
	(iter a 0)))


(define (product term a next b)
  (if (> a b)
	1
	(* term (product term (next a) next b))))



(define (f x y)
  (define (f-helper a b)
	(+ (* x (square a))
	   (* y b)
	   (* a b)))
  (f-helper (+ 1 (* x y))
			(- 1 y)))

;; bad practice, we use define only for internal procedures
(define (f x y)
  (define a (+ 1 (+ x y)))
  (define b (- 1 y))

  (+ (* x (square a))
	 (* y b)
	 (* a b)))

(define (f x y)
  ((lambda (a b) (+ (* x (square a))
					(* y b)
					(* a b))) (+ 1 (* x y) (- 1 y))))

(define (f x y)
  (let ((a (+ 1 (* x y)))
		(b (-1 y)))
	(+ (* x (square a))
		  (* y b)
		  (* a b))))


---------------------------------------
(define (f g)
  (g 2))
(f square)

(f (lambda (z) (* z (+ z 1))))

(f f)
---------------------------------------


(define (close-enough? a b)
	(< (abs (- a b)) 0.001))

(define (search f neg-point pos-point)
	(let ((midpoint (average neg-point pos-point)))
		(if (close-enough? neg-point pos-point)
			midpoint
			(let ((test-value (f midpoint))
				(cond ((positive? test-value)
					   (search f neg-point test-value))
					  ((negative? test-value) 
					  	(search f test-value pos-point))
					  (else midpoint)))))))

(define (half-interval-method f a b)
	(let ((a-value (f a))
		 ((b-value (f b))))
		 (cond ((and (negative? a-value) (positive? b-value))
		 		(search f a-value b-value))
			   ((and (positive? a-value) (negative? b-value))
			   	(search f b-value a-value))
			   (else (error "values are note opposites sign" a b)))))


(define tolerance 0.00001)

(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) tolerance))
	(define (try guess)
		(let ((next (f guess)))
			(if (close-enough? guess next)
				next
				(try next))))
	(try first-guess))

(fixed-point (lambda (x) (+ 1 (/ 1 x)) 1.0))

(define (cont-frac n d k) 
	(define (loop result term)
		(if (= term 0) )
		(loop (/ (n term) (+ (d term) result)) (- term 1)))

	(loop 0 k))


(cont-frac (lambda (i) 1.0)
		   (lambda (i) 1.0)
		   k
)


(car cons (x y))
(car (lambda (m) (m x y)))
((lambda (m) (m x y)) (lambda (p q) p))
((lambda (p q) p) x y)
(x)

;;2.5
(define (cons x y)
	(* (exp 2 x) (exp 3 y)))

(define (reminder-division n divisor)
	(define (try n power)
		(if (divides? divisor n)
			(try (/ n divisor) (+ power 1)))
			(power)
		))
	(try n 0))

(define (car z) (reminder-division z 2))

(define (cdr z) (reminder-division z 3))

;;2.6

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n) 
	(lambda (f) (lambda (x) (f ((n f) x)))))

one:
	(lambda (f) (lambda (x) (f x)))

(define one (lambda (f) (lambda (x) (f x))))

two:
	(lambda (f) (lambda (x) (f f x)))

(define two (lambda (f) (lambda (x) (f (f x)))))

three:

(define three (lambda (f) (lambda (x) (f (f (f x))))))


(define (+ a b)
	(lambda (f)
		(lambda (x)
			((a f) ((b f) x)))))

;;2.7
;; interval may contain negative number
(define (make-interval a b)
	(if (< a b)
		(cons a b)
		(cons b a)))

(define (lower-bound i) (car i))

(define (upper-bound i) (cdr i))

(define (add-interval a b)
	(make-interval (+ (lower-bound a) (lower-bound b))
				   (+ (upper-bound a) (upper-bound b))))

(define (reverse-interval n)
	(let ((l (lower-bound n))
		 (u (upper-bound n)))
		(if (and (< l 0) (> u 0))
		  (make-interval (/ 1 l) (/ 1 u))
		  (make-interval (/ 1 u) (/ 1 l)))))

(define (neg-interval n)
	(let ((low (lower-bound n)
		  (upper (upper-bound n)))
		  (if (and (< low 0) (< upper 0))
			  (make-interval (- low) (- upper))
			  (make-interval (- upper) (- low))))))

(define (sub-interval a b)
	(add-interval a (neg-interval b)))

(define (multiply-interval a b)
	(let ((p1 (* (lower-bound a) (lower-bound b)))
		 (p2 (* (lower-bound a) (upper-bound b)))
		 (p3 (* (upper-bound a) (upper-bound b)))
		 (p4 (* (upper-bound a) (lower-bound b))))
		(make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(define (div-interval a b)
	(if (<= (* (lower-bound b) (upper-bound b)) 0)
		(error "divide error" b)
		(multiply-interval a (reverse-interval b))))

(define (equal-interval? a b)
	(and (= (lower-bound a) (lower-bound b))
		 (= (upper-bound b) (upper-bound b))))

(define (width a)
	(abs (average (lower-bound a) (upper-bound a))))

(define (print-interval a)
	(newline)
	(display "(")
	(display (lower-bound a))
	(display ", ")
	(display (upper-bound a))
	(display ")"))

;; test
(print-interval (make-interval 1 6))
(print-interval (add-interval (make-interval 1 6) (make-interval 3 5)))
(print-interval (sub-interval (make-interval 1 5) (make-interval 2 77)))
(print-interval (div-interval (make-interval 1 5) (make-interval 2 77)))

;; test width
(= (+ (width (make-interval 1 4)) (width (make-interval 2 56)))
   (width (add-interval (make-interval 1 4) (make-interval 2 56))))

;;2.12
(define (make-center-percent c p)
	(make-interval (- c (* c p)) (+ c (* c p))))

(define (center i)
	(average (lower-bound i) (upper-bound i)))

(define (percent i)
	(abs (/ (width i) (center i))))

;; 2.13

(define (list-ref items n)
	(if (= n 0)
		(car items)
		(list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 25 36))

(list-ref squares 2)

(define (length items)
	(if (null? items)
		0
		(+ 1 (length (cdr items)))))

(define (length items)

(define (iter result l)
	(if (null? l)
		result
		(iter (+ 1 result) (cdr l))))
	(iter 0 items))

(define odds (list 1 3 5 7))

(length odds)

(define (append list1 list2)
	(if (null? list1)
		list2
		(cons (car list1) (append (cdr list1) list2))))

;;ex2.17
(define (last-pair items)
	(if (null? (cdr items)) 
		items
		(last-pair (cdr items)))

(define (last-pair items)
	(define (iter lists term)
		(if (null? lists)
			term
			(iter (cdr lists) (car lists))))
	(iter items nil))

;;ex2.18

(define (first-n-items n items)
	(if (null? items)
		""
		(cons (car items) (first-n-iterms (- n 1) (cdr items)))))

(define (head items)
	(first-n-items (- (length items) 1) items))

(define (reverse lists)
	(if (null? lists)
		""
		(cons (last-pair lists) (reverse (head lists)))))

;; after read answer
(define (reverse lists)
	(if (null? (cdr lists)
		lists
		(append (reverse (cdr lists)) (list (car lists))))))

;; 2.19
(define us-coins (list 25 50 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-lists)
  (cond ((= amount 0) 1) 
        ((or (< amount 0) (= (length coin-lists) 0)) 0)
    		(else (+ (cc (- amount (car coin-lists)) coin-lists)
          		   (cc amount (cdr coin-lists))))))
          		 
(cc 100 us-coins)
;;(cc 100 uk-coins)

;; 2.20

(define (odd? n)
  (not (even? n)))

(define (same-parity a . b)
  (define (pick-even items)
    (if (null? items)
        1000
        (if (even? (car items)
                   (cons (car items) (pick-even (cdr items)))
                   (pick-even (cdr items))))))
                 
  (define (pick-odd items)
    (if (null? items)
        1000
        (if (odd? (car items)
                   (cons (car items) (pick-odd (cdr items)))
                   (pick-odd (cdr items))))))
      
      
  (if (null? b)
      a
      (if (even? a)
          (cons a (pick-even b))
          (cons a (pick-odd b))))) 

(same-parity 1 2 3 4 5 6 7 8 9)

;; after read the ansnwer

(define (same-parity first . rest)
	(define (iter source result reminder-val)
		(if (null? source)
			result
			(iter (cdr source)
				  (if (= (reminder (car source) 2) reminder-val)
					(append (list (car source) result))
					result)
				 reminder-val)))

	(iter rest (list first) (reminder first 2)))



(define (scale-list items 10)
	(if (null? items)
		nill
		(cons (* (car items) 10)
			  (scale-list (cdr items) 10))))

(define (map items proc)
	(if (null? items)
		nill
		(cons (proc (car items))
			  (map (cdr items) proc))))
;;2.21

(define (square-list items)
	(if (null? items)
		items
		(cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
	(map items (lambda (x) (* x x))))

;; iteractive process
(define (square-list items)
	(define (iter result resoures)
		(if (null? resources)
			result
			(iter (append result (list (car resources))) (cdr resources))))
	(iter (list (car items)) (cdr items)))

;; 2.23
(define (for-each proc items)
	(if (null? items)
		(newline)
		((lambda (items proc)
			((proc (car items))
			(newline)
			(for-each proc (cdr items)))) items proc)))
	

(define (for-each proc items)
	(cond ((null? items) (true))
		  (else 
			(proc (car items))
			(newline)
			(for-each proc (cdr items)))))

(define (for-each proc items)
	(if (not (null? items)) 
		(proc (car items))
		(for-each proc (cdr items))))


;; 2.25

;; want to get 7
(define seq (list 1 3 (list 5 7) 9))
	(cdr (car (cdr (cdr seq))))

(define seq (list (list 7)))
	(car (car seq))

(define seq (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
	(cdr (cdr (cdr (cdr (cdr seq)))))



;; 2.27

(define (reverse lists)
	(if (null? (cdr lists)
		lists
		(append (reverse (cdr lists)) (list (car lists))))))

(define (deep-reverse lists)
	(cond ((not (pair? lists) lists))
		  (else (cons (deep-reverse (cdr lists)) (deep-reverse (car lists))))))

;; 2.28

;; think, why this one doesn't work
(define (fringe tree)
	(if (null? tree)
		tree
		(let ((first (car tree)))
		(append ((if (pair? first)
					(fringe first)
					(list first))
				(fringe (cdr tree)))))))

;; this one works
(define (fringe x)
	(if (null? x)
		x
		(let ((first (car tree)))
			(if (pair? first)
				(append (fringe first) (fringe (cdr x)))
				(cons first (fringe (cdr x)))))))

;; let's do a iteractive one
;; doesn't work
(define (fringe x)
	(define (iter result resource)
		(if (null? resource)
			result
			(if (pair? (car resource)
				(iter (append result (iter (list ) (car resource))) (cdr resource))
				(iter (append result (list (car resource))) (cdr resource)))))))
;; fix
(define (fringe x)
	(define (iter result resource)
		(if (null? resource)
			result
			(iter (append result 
					(if (pair? (car resource))
						(iter (list ) (car resource))
						(list (car resource))
					(cdr resource))))))
	(iter x (list)))


;;2.29
-----------------------------------------
(define (make-mobile left right)
	(list left right))
	
(define (make-branch length structure)
	(list length structure))

(define (left-branch m)
	(car m))

(define (right-branch m)
	(car (cdr m)))

(define (branch-length b)
	(car b))

(define (branch-structure b)
	(car (cdr b)))

(define (is-structure? m)
	(pair? m))
-----------------------------------------
(define (make-mobile left right)
	(cons left right))
	
(define (make-branch length structure)
	(cons length structure))

(define (left-branch m)
	(car m))

(define (right-branch m)
	(cdr m))

(define (branch-length b)
	(car b))

(define (branch-structure b)
	(cdr b))

(define (is-structure? m)
	(pair? m))
-----------------------------------------

(define (mobile-weight m)
	(+ (branch-weight (left-branch m))
		(branch-weight (right-branch m))))

(define (branch-weight b)
	(let ((s (branch-structure b)))
		(if (is-structure? s)
			(mobile-weight s)
			s)))

(define (branch-balance? b)
	(let ((s (branch-structure b)))
	(if (is-structure? s) 
		(blanced? s) 
		true)))

(define (branch-torque b)
	(* (branch-length b) 
		(branch-weight b)))

(define (balanced? m)
	(if (not (is-structure? m))
		true
		(let ((left (left-branch m)))
			 ((right (right-branch m)))
			 (and (= (branch-torque left) (branch-torque right))
				(branch-balance? (branch-structure left)) 
				(branch-blanced? (branch-structure right))))))



;;
(define (scale-tree t factor)
	(if (not (pair? t))
		(list (* t factor))
		(append (scale-tree (car t) factor)
			(scale-tree (cdr t) factor))))

(define (scale-tree tree factor)
	(cond ((null? tree) nil)
		  ((not (pair? tree)) (* tree factor))
		  (else (cons (scale-tree (car tree) factor)
		  				(scale-tree (cdr tree) factor)))))

(define (scale-tree tree factor)
	(map (lambda (sub-tree)
			(if (pair? sub-tree)
				(scale-tree sub-tree factor)
				(* sub-tree factor)))
		tree))

;;2.30
(define (square-tree t)
	(cond ((null? t) t)
		  ((not (pair? t)) (* t t))
		  (else (cons (square-tree (car t))
		  			  (square-tree (cdr t))))))

(define (square-tree t)
	(map (lambda (tree)
			(if (pair? tree) 
				(square-tree tree)
				(* tree tree)))
	t))

;;2.31
(define (tree-map proc tree)
	(map (lambda (tree)
		(if (pair? tree)
			(tree-map proc tree)
			(proc tree)
	tree))))

;;2.32
(define (subsets set)
	(if (null? s)
		(list nil)
		(let ((rest (subsets (cdr s))))
			(append rest (map (lambda (m) (cons (car s) m)) rest)))))





;; jimmy
(define (filter predicate seq)
	(cond ((null? seq) nil)
		  ((pair? seq) (filter predicate seq))
		(cons (if (pair? (car seq)) (filter predicate (car seq)) 
									if (predicate (car seq)) (car seq) nil) 
			  (filter predicate (cdr seq)))))

(define (filter predicate seq)
	(cond ((null? seq) nil)
		  ((predicate (car seq))
		  	(cons (car seq) (filter predicate seq)))
		  (else (filter predicate (cdr seq)))))

;; jimmy
(define (accumulate op initial seq)
	(if (null? seq) 
		initial
		(accumulate op (op initial (car seq)) (cdr seq))))


(define (accumulate op initial seq)
	(if (null? seq)
		initial
		(op (car seq) (accumulate op initial (cdr seq)))))

(define (enumerate-interval low high)
	(if (> low high)
		nil
		(cons low (enumerate-interval (+ low 1) high))))

;;jimmy
(define (enumerate-tree tree)
	(cond ((null? tree) tree)
		  ((not (pair? tree)) (list tree))
		  (else (append (enumerate-tree (car tree)) (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
	(accumulate + 0 (map square (filter odd? (enumerate-tree tree)))))

(define (even-fibs n)
	(accumulate cons nil (filter even? (map fib (enumerate-interval 0 n)))))


;;2.33
;; jimmy
(define (map p sequence)
	(if (null? sequence)
		sequence
		(cons (p (car sequence)) (map p (cdr sequence)))))

(define (map p sequence)
	(accumulate (lambda (x y) (cons (p x) y)) nil sequence))


;;jimmy
(define (append seq1 seq2)
	(accumulate cons nil (list (car seq1) (cdr seq1) seq2)))

;;jimmy
(define (length seq)
	(accumulate incr 0 seq))

;;2.34

;; jimmy
(define (horner-eval x coefficient-seq)
	(accumulate (lambda (this-coeff higher-terms) (+ (* x higher-term) this-coeff))
				0
				coefficient-seq))


;;2.35
;; old count-leaves
(define (count-leaves tree)
	(if (not (pair? tree)) 
		1
		(+ (count-leaves (car tree))
		   (count-leaves (cdr tree)))))

;; jimmy
(define (count-leaves tree)
	(accumulate + 0 (map (lambda (t) 
	                       (cond 
							   ;; why don't need to check if the node is null, because if we don't use selector 'car' and 'cdr' then there will be not nil, right?
							   ;; or may be the 'map' did it for me ?
						   		;;((null? t) t) 
								 ((pair? t) (count-leaves t))
								 (else 1))) tree)))


;;2.36
(define (accumulate-n op init seqs)
	(if (null? (car seqs))
		nil
		(cons (accumulate op init (map car seqs))
			  (accumulate-n op init (map cdr seqs)))))


;;2.37
;; todo
(define (dot-product v w)
	(accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
	(map <> m))

;;2.38

(define (fold-left))

2/3
1/6
(list 1 (list (2 (list 3 nil)))

(1 (2 (3)))

(((1) 2) 3)

;;2.39
;; fold right

(define (reverse seq)
	(accumulate (lambda (x y) (append y (list x))) nil sequence))

;; fold left
(define (reverse seq)
	(fold-left (lambda (x y) (cons y x)) nil sequence))



;;------------------

(define (emulator-interval low high)
	(if (> low high)
		'()
		(cons low (emulator-interval (+ low 1) high))))

(accumulate
  append 
  (list)
  (map (lambda (i) 
         (map (lambda (j) 
                (list i j)) (emulator-interval 0 (- i 1)))) 
       (emulator-interval 0 6))))

	   ↓

	   
(define (emulator-interval low high)
	(if (> low high)
		'()
		(cons low (emulator-interval (+ low 1) high))))

(define (flatmap proc items)
  (accumulate 
              append
              (list)
              (map proc items)))

(define (filter-sum-prime items)
  (filter (prime? (+ (car items) (car (cdr items)))) items))

(define (print-triple items)
  (map (lambda (i) (append i (+ (car i) (car (cdr i))))) items))

(define (sum-prime-pair n)
	  (print-triple 
      (filter-sum-prime 
        (flatmap 
         (lambda (i) 
    				 (map (lambda (j)  (list i j)) 
    				      (emulator-interval 0 (- i 1)))) 
         (emulator-interval 0 n)))))

(sum-prime-pair 8)


;; after change abstraction level
;;==============
(define (emulator-interval low high)
	(if (> low high)
		'()
		(cons low (emulator-interval (+ low 1) high))))

(define (flatmap proc items)
  (accumulate 
	  append
	  (list)
	  (map proc items)))

(define (prime-pair? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-sum-triple pair)
  (append pair (list (+ (car pair) (cadr pair)))))

(define (sum-prime-pair n)
	  (map make-sum-triple 
		  (filter prime-pair?
			  (flatmap (lambda (i) 
						 (map (lambda (j) (list i j)) 
							 (emulator-interval 1 (- i 1))))
					   (emulator-interval 1 n)))))


;;================
(define (remove-item x items)
	(filter (lambda (i) (not (= i x))) items))

(define (permutation seq)
		(flatmap (lambda (i) 
					(map ((lambda j) 
						(list i (car j) (car (cdr j)))) (permutation (remove-item i seq)))) 
				 seq)))

;; a lot of problem, i change it to...

(define (remove-item x items)
	(filter (lambda (i) (not (= i x))) items))

(define (permutation seq)
		(flatmap
		  (lambda (i) 
				(map (lambda (j)  (cons i j)) (permutation (remove-item i seq)))) 
		  seq))



;; 2.40

(define (unique-pairs n)
	(flatmap (lambda (i)
    				(map (lambda (j)
    					(list i j))
    					(emulator-interval 1 (- i 1))))
    			 (emulator-interval 1 n)))

;;2.41

(define (sum-triple-equal-to t s)
	(= (+ (car t) (car (cad t)) (car (car (cad t)))) s))

(define (exp-seq seq n)
	(if (<= n 1) 
		seq
		(flatmap (lambda (s) 
					(flatmap (lambda (d) (cons d s)) seq)) 
				 (exp-seq seq (- n 1)))))


(define (sum-triples n s)
	(if (< (* n 3) s)
		(error "wrong condition.")
		(filter (lambda (t) (sum-triple-equal-to t s))
			(flatmap (lambda (i)
						(map (lambda (s) (enulator 1 n)
						(list i () ()))
					 (exp-seq (emulator 1 n) 3)))))))


;; after debug

(define (sum-triple-equal-to t s)
	(= (+ (car t) (car (cdr t)) (car (cdr (cdr t)))) s))

(define (exp-seq seq n)
	(if (<= n 1) 
		(map list seq)
		(flatmap (lambda (s) 
					(map (lambda (d) (cons d s)) seq)) 
				 (exp-seq seq (- n 1)))))


(exp-seq (list 1 2 3 4) 3)

(define (sum-triples n s)
	(if (< (* n 3) s)
		(error "wrong condition.")
		(filter (lambda (t) (sum-triple-equal-to t s))
					 (exp-seq (emulator-interval 1 n) 3))))

(sum-triples 8 7)


;;the answer is




;; 2.42
;; queens

(define (exp-seq seq n)
	(if (<= n 1) 
		(map list seq)
		(flatmap (lambda (s) 
					(map (lambda (d) (cons d s)) seq)) 
				 (exp-seq seq (- n 1)))))


(exp-seq (list 1 2 3 4) 3)

(define (sum-triples n s)
	(if (< (* n 3) s)
		(error "wrong condition.")
		(filter (lambda (t) (sum-triple-equal-to t s))
					 (exp-seq (emulator-interval 1 n) 3))))

(sum-triples 8 7)

(define (make-seq n e)
  (cond ((= n 0) (list))
         ((= n 1) (list (list e)))
         (else  (let ((rest-seqs (make-seq (- n 1) e)))
                     (map (lambda (seq1) (append (list e) seq1))
                       (cons (car rest-seqs) rest-seqs))))))
       
(define (empty-board size)
  (make-seq size 0))


(define (adjoin-position))
(define (safe?))
(define (empty-board))

(define (queens board-size)
	(define (queen-cols k)
		(if (= k 0)
			(list empty-board)
			(filter
				(lambda (positions) (safe? k positions))
					(flatmap
						(lambda (rest-of-queens)
							(map (lambda (new-row)
									(adjoin-position new-row
													 k
													 rest-of-queens))
								 (enumerate-interval 1 board-size)))
					(queen-cols (- k 1))))))

	(queen-cols board-size))


;; 2.46
(define (make-vect x y) (cons x y))
(define (xcor-vect vec) (car vec))
(define (ycor-vect vec) (cdr vec))

(define (eq-vect? v1 v2)
  (and (= (xcor-vect v1) (xcor-vect v2))
	   (= (ycor-vect v1) (ycor-vect v2))))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
			 (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
			 (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s vec)
  (make-vect (* s (xcor-vect vec))
			 (* s (ycor-vect vec))))

;; TDD test
(define (ensure b err-msg)
  (if (not b) (error err-msg)))

(define (ensure-all list-of-tests-and-messages)
  (cond ((null? list-of-tests-and-messages) true)
		(else
		  (ensure (car list-of-tests-and-messages)
				  (car (cdr list-of-tests-and-messages)))
		  (ensure-all (cdr (cdr list-of-tests-and-messages))))))

(define v1 (make-vect 2 3))
(define v2 (make-vect 5 8))

(ensure-all
  (list (= (xcor-vect (make-vect 3 4)) 3) 
		"x"
		(= (ycor-vect (make-vect 3 4)) 4)
		"y"
		(eq-vect? (make-vect 7 11) (add-vect v2 v1))
		"add"
		(eq-vect? (make-vect 3 5) (sub-vect v2 v1))
		"sub"
		(eq-vect? (make-vect 10 16) (scale-vect 2 v2))))
;; end of TDD test

;;2.47

;;
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (frame-origin f) (car f)) 
(define (frame-edge1 f) (cadr f)) 
(define (frame-edge2 f) (caddr f))

(define (frame-coord-map frame)
  (lambda (v)
	(add-vect
	  (origin-frame frame)
	  (add-vect (scale-vect (xcor-vect v) 
							(edge1-frame frame))
				(scale-vect (ycor-vect v) 
							(edge2-frame frame))))))

;;2.48
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define (segments->painter segment-list)
  (lambda (frame)
	(for-each 
	  (lambda (segment)
		(draw-line
		  ((frame-coord-map frame) (start-segment segment))
		  ((frame-coord-map frame) (end-segment segment))
		  segment-list)))))

;;2.53
(a b c)
unbound symbol
((y1 y2))
(y1 y2)
#f
#f
(red shoes blue socks)


;;2.54
(define (equal? x y)
  (cond ((and (pair? x) (pair? y))
			  (and (equal? (car x) (car y)) 
			       (equal? (cdr x) (cdr y))))
		((and (not (pair? x)) (not (pair? y))) (eq? x y))
		(else #f)))

;;2.55

;; 2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
                         (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

;; 2.60
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) (element-of-set? x (cdr set)))
        (else (element-of-set? x (cdr set)))))

;;2.61
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< x (car set)) (cons x set))
        ((> x (car set)) (cons (car set) (adjoin-set x (cdr set))))
        (else set)))

;;2.62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set (car set1) set2) (union-set (cdr set1) set2))
        (else (union-set (cdr set1) (adjoin-set (car set1) set2)))))


(define (make-leaf symbol weight)
  (list 'leaf symbol weight))


(define (leaf? object)
  (equal? 'leaf (car object)))

(define (symbol-leaf x) ((car (cdr x))))
  
(define (weight-leaf x) ((car (cdr (cdr x)))))

;(define l (make-leaf 'A 22))


(weight-leaf (make-leaf 'A  22))
(symbol-leaf (make-leaf 'A  22))

  
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
  

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (car (cdr tree)))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (car (cdr(cdr tree))))))
    
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (car (cdr(cdr(cdr tree))))))


(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? tree)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
             (if (leaf? next-branch)
                 (cons (symbol-leaf next-branch) (decode-1 bits tree))
                 (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit"))))
