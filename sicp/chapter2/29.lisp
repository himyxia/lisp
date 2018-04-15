(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))


(define (total-weight mobile) 
  (cond ((null? mobile) 0)
        ((not (list? mobile)) mobile)
        (else (let ((left (left-branch mobile))
                    (right (right-branch mobile)))
                  (+ (total-weight (branch-structure left))
                     (total-weight (branch-structure right)))))))
               
               
  (define (prod branch)
    (cond ((null? branch) 0)
          ((not (list? branch)) branch)
          (else (let ((s (branch-structure branch)))
                     (if (not (list? s))
                         (* (branch-length branch) s)
                         (+ (branch-length branch) (total-weight s)))))))

(define (balanced? mobile)
        (let ((left (left-branch mobile))
              (right (right-branch mobile)))
          (= (prod left) 
             (prod right)))) 



## if we change the mobile representation to

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

(define (total-weight mobile) 
  (cond ((not (pair? mobile)) mobile)
        (else (let ((left (left-branch mobile))
                    (right (right-branch mobile)))
                  (+ (total-weight (branch-structure left))
                     (total-weight (branch-structure right)))))))
               
               
(define (prod branch)
  (let ((s (branch-structure branch)))
        (if (not (pair? s))
          (* (branch-length branch) s)
          (+ (branch-length branch) (total-weight s)))))

(define (balanced? mobile)
        (let ((left (left-branch mobile))
              (right (right-branch mobile)))
          (= (prod left) 
             (prod right)))) 
           
    
