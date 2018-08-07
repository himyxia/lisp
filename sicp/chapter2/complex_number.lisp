;; API:      `add-complex`, `sub-complex`, `mul-complex`, `div-complex`
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z2) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; version 1
;; advantadge:
	;; add-complex, sub-complex, mul-complex, div-complex 
	;; will work with either hacker A's representation or B's representation

;; problem: 
	;; altought you seems like can get different part of complex number
	;; but underlying you can only choose one of the implementation(either `rectangle` or `polar`)
	;; name conflict

;; `add-complex`, `sub-complex`, `mul-complex`, `div-complex`
;;                             ↓
;;         real-part, imag-part, magnitude, angle      
;;                             ↓
;;    hackerA's representation or hackerB's representation 

;;;;;; hacker A
(define (make-from-real-image x y) (cons x y))
(define (make-from-mag-ang r a) (cons (* r (cos a)) (* r (sin a))))

(define (real-part z) (car z))
(define (imag-part z) (cdr z))

(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))

(define (angle z)
  (atan (imag-part z) (real-part z)))


;;;;;; hacker B
(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))
      
(define (make-from-mag-ang r a) (cons r a))

(define (real-part z) 
  (* (magnitude z) (cos (angle z))))

(define (imag-part z) 
  (* (magnitude z) (sin (angle z))))

(define (magnitude z) (car z))

(define (angle z) (cdr z))

;; version 2: explicit dispatch
;; `add-complex`, `sub-complex`, `mul-complex`, `div-complex`
;;                         ↓
;;         real-part, imag-part, magnitude, angle      
;;                         ↓
;;                        type
                       ;;real-part-rectangular 
                       ;;imag-part-rectangular
                       ;;real-part-polar 
                       ;;imag-part-polar
                       ;;angle-part-rectangular
                       ;;angle-part-polar 
                       ;;magnitude-part-polar (contents z)))
                       ;;magnitude-part-rectangular (contents z)))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

;; get type-tag
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "bad tagged datum")))
    
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "bad tagged datum")))
    
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-ploar r a))

;;;; dispath type  
(define (real-part z)
  (cond ((rectangular? z)
                       (real-part-rectangular (contents z)))
        ((polar? z)
                       (real-part-polar (contents z)))
        (else (error "unknown type"))))
      
(define (imag-part z)
  (cond ((rectangular? z)
                       (imag-part-rectangular (contents z)))
        ((polar? z)
                       (imag-part-polar (contents z)))
        (else (error "unknown type"))))
      
(define (magnitude z)
  (cond ((rectangular? z)
                       (magnitude-part-rectangular (contents z)))
        ((polar? z)
                       (magnitude-part-polar (contents z)))
        (else (error "unknown type"))))
      
(define (angle z)
  (cond ((rectangular? z)
                       (angle-part-rectangular (contents z)))
        ((polar? z)
                       (angle-part-polar (contents z)))
        (else (error "unknown type"))))

;::; hacker A
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
  
(define (angle-rectangular z)
  (atan (image-part-rectangular z)
        (real-part-rectangular z)))

(define (make-from-real-image-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular x y)
  (attach-tag 'rectangular 
              (cons (* r (cos a)) (* r (sin a)))))

;::; hacker B
(define (real-part-polar z) 
  (* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part-polar z) 
  (* (magnitude-polar z) (sin (angle-polar z))))

(define (magnitude-polar z)
  (car z))

(define (angle-polar z)
  (cdr z))
  
(define (make-from-real-image-polar x y)
  (attach-tag 'polar 
              (cons (sqrt (+ (square x) (square y))) 
                    (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))


;;;;;;;;;;;;;;;;; version 3: data-directed style
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "bad tagged datum")))

;; TODO
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
       (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (error "no method for these types" (list op type-tags))))))

;;;; hacker A
(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-image x y) (cons x y))
  
  (define (magnitude z) 
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-image 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
     
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
  
;;;; hacker B
(define (install-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a)
    (cons r a))
  
  (define (real-part z) 
    (* (magnitude z) (cos (angle z))))
  
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  
  (define (make-from-real-image x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-image 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
     
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


;; jimmy, provide API
;;;; make-from-mag-ang
;;;; make-from-real-imag
;;;; real-part
;;;; imag-part
;;;; magnitude
;;;; angle


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; version 4, message passing
(define (apply-generic op arg) (arg op))

(define (make-from-real-image x y)
  (define (dispatch op)
	(cond ((eq? op 'real-part) x)
		  ((eq? op 'imag-part) y)
		  ((eq? op 'magnitude)
		   	(sqrt (+ (square x) (square y))))
		  ((eq? op 'angle) (atan x))
		  (else (error "unknow operation" op))))
	dispatch)


(define (make-from-mag-ang m a)
  (define (dispatch op)
	(cond ((eq? op 'real-part) (* r (cos a)))
		  ((eq? op 'imag-part) (* r (sin a)))
		  ((eq? op 'magnitude) m)
		  ((eq? op 'angle) a)
		  (else (error "unkonw operation" op))))
  (dispatch))
