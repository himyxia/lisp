(define make-point cons)
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define make-seg cons)
(define (start-seg s) (car s))
(define (end-seg s) (cdr s))


(define (midpoint-seg seg)
  (let ((start (start-seg seg))
       (end (end-seg seg)))
  (make-point (/ (+ (x-point start)
                 (x-point end)) 2)
              (/ (+ (y-point start)
                    (y-point end)) 2))))
                  
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

// using two point to represent rectangle
(define make-rect cons)

(define (start-rect rect)
  (car rect))

(define (end-rect rect)
  (cdr rect))

(define (perimeter rect)
  (let ((start (start-rect rect))
		(end (end-rect rect))) 
	(* 2 (+ (abs (- (x-point start)
					(x-point end)))
			(abs (- (y-point start)
					(y-point end)))))))


(define (area rect)
  (let ((start (start-rect rect))
		(end (end-rect rect))) 
	(* (abs (- (x-point start)
					(x-point end))) 
	   (abs (- (y-point start)
					(y-point end))))))

// using two-segment to represent 
(define make-rect cons)

(define (start-rect rect)
  (start-seg (car rect))

(define (end-rect rect)
  (make-point (x-point (end-seg (cdr rect))) 
			  (y-point (end-seg (car rect)))))


(define test-rect (make-rect (make-seg (make-point 1 1) (make-point 1 8)) 
							 (make-seg (make-point 1 1) (make-point 22 1))))
(area test-rect)
(perimeter test-rect)
