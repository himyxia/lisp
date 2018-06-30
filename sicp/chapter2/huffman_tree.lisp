(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (car (cdr x)))

(define (weight-leaf x) (car (cdr (cdr x))))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
  
  
(define (left-branch tree) (car tree))
(define (right-branch tree) (car (cdr tree)))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (car (cdr (cdr tree)))))
  
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (car (cdr (cdr (cdr tree))))))



(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch current-branch (car bits))))
           (if (leaf? next-branch)
               (cons (symbol-leaf next-branch) 
                     (decode (cdr bits) tree))
               (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch tree bit)
  (cond ((= bit 0) (left-branch tree))
        ((= bit 1) (right-branch tree))
        (else (error "bad bit"))))


(define (adjoin-set x set)

  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))


(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
           (adjoin-set (make-leaf (car pair) (car (cdr pair)))
                       (make-leaf-set (cdr pairs))))))


(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree 
                                  (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1)
                                                  (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define (contains x set)
  (cond ((null? set) #f)
        ((eq? x (car set)) #t)
        (else (contains x (cdr set)))))

(define (encode symbols tree)
  (if (null? symbols)
      '()
      (append (encode-symbol (car symbols) tree) 
              (encode (cdr symbols) tree))))

(define (encode-symbol symbol tree)
  (cond ((not (contains symbol (symbols tree))) 
              (display "symbol not found in tree"))
        ((leaf? tree) '())
        (else (let ((left (left-branch tree))
                      (right (right-branch tree)))
              (if (contains symbol (symbols left))
                  (cons '0 (encode-symbol symbol left))
                  (cons '1 (encode-symbol symbol right)))))))


;;(encode '(A D A B B C A) sample-tree)
;;(encode-symbol 'C sample-tree)
(decode sample-message sample-tree)
    
    
    
    
    
    
 (define (generate-huffman-tree pairs)
   (successive-merge (make-leaf-set pairs))) 
 
;; make-code-tree 

 (define (successive-merge leaf-pairs)
   
   (define (successive-merge-1 first remains-pairs)
     ;;(display first)
     (if (null? remains-pairs) 
         (change-leaf-to-tree first)
         (successive-merge-1 (make-code-tree first (car remains-pairs)) (cdr remains-pairs))))
         
    
    (display leaf-pairs)
    (if (null? leaf-pairs) 
        '()
         (successive-merge-1 (car leaf-pairs) (cdr leaf-pairs))))
    
    
(define (change-leaf-to-tree leaf)
  (if (leaf? leaf)
    (list '() '() (symbols leaf) (weight-leaf leaf))
    leaf))


    
;;(generate-huffman-tree (list (cons 'D 1) (cons 'C 1) (cons 'B 2) (cons 'A 4)))


(generate-huffman-tree (list (list 'D 1) (list 'C 1) (list 'B 2) (list 'A 4)))


;;(generate-huffman-tree (list (list 'D 1)))



                     
;;(make-leaf-set (list (list 'D 1) (list 'C 1) (list 'B 2) (list 'A 4)))

(generate-huffman-tree (list (list 'A 2) 
                             (list 'NA 16) 
                             (list 'BOOM 1) 
                             (list 'SHA 3) 
                             (list 'GET 2) 
                             (list 'YIP 9) 
                             (list 'JOB 2) 
                             (list 'WAH 1) 
                             ))

(define song (generate-huffman-tree (list (list 'A 2) 
                             (list 'NA 16) 
                             (list 'BOOM 1) 
                             (list 'SHA 3) 
                             (list 'GET 2) 
                             (list 'YIP 9) 
                             (list 'JOB 2) 
                             (list 'WAH 1) 
                             )))
                           
