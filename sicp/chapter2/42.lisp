(define (get-col k cols)
  (cond ((or (< k 1) (> k (length cols))) (list))
        ((= k 1) (car cols))
        (else (get-col (- k 1) (cdr cols)))))
      
(define (make-col this-row this-col board-size)
  (list this-row this-col board-size))

(define (get-row col)
  (car col))

(define (get-this-col col)
  (car (cdr col)))

(define (get-board-size col)
  (car (cdr (cdr col))))

(define (accumlator op init seq)
  (if (null? seq) 
      init
      (op (car seq) (accumlator op init (cdr seq)))))
  
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      (list)
      (cons (accumlator op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))
  
    
(define (flatmap proc seq)
  (accumlator append (list) (map proc seq)))

(define (enumrate-interval low high)
  (if (> low high) 
      (list)
      (append (list low) (enumrate-interval (+ low 1) high))))

;;(gen-seqs-by-cols (list (list 1 8) (list 2 8)(list 1 8)))

   (define (equal-or-oposite x y)
     (if (or (= x 0) (= y 0)) 
         #f 
         (or (= x y) (= x (- y)))))

  (define (safe? k positions)
    (cond ((null? positions) #t)
          ((= 1 (length positions)) #t)
          (else  (let ((latest-col (get-col k positions))) 
                      (and (= 0 (length (filter (lambda (x) (> x 1)) 
                                               (accumulate-n + 0 (gen-seqs-by-cols positions))))) 
                          (= 0 (length (filter (lambda (x) (equal-or-oposite (- (get-row x) (get-row latest-col)) (- (get-this-col x) (get-this-col latest-col)))) positions))))))))
                        
  (define (gen-seq-by-col col)
    (accumlator cons (list) (map (lambda (x) (if (= x (get-row col)) 1 0)) (enumrate-interval 1 (get-board-size col)))))
  
  (define (gen-seqs-by-cols cols)
      (accumlator cons (list) (map gen-seq-by-col cols)))
    
(define (queens board-size)
  (define empty-board (list (list)))
  
  (define (adjoin-position new-row k rest-of-queens)
    (let ((new-col (make-col new-row k board-size)))
      (if (null? rest-of-queens)
          (list new-col)
         (append rest-of-queens (list new-col)))))
      
  (define (queen-cols k)
    (if (= k 0)
        (list (list))
        (filter (lambda (positions) (safe? k positions))
                (flatmap (lambda (rest-of-queens)
                           (map (lambda (new-row) (adjoin-position new-row k rest-of-queens)) (enumrate-interval 1 board-size)))
                         (queen-cols (- k 1))))))
  (queen-cols board-size))

(length (queens 8))
;;(gen-seq-by-col (list 26 8))

;;(adjoin-position 1 8 (list))



;;(adjoin-position 2 1 (list))

;;() -> (1) -> ((1 0) (1 1))
;;
;;
;;empty-board
;;
;;(map (lambda (x) ()) ())
;;
;;() -> (1 0 0 0 0 0 0 0) ->  (1 0 0 0 0 0 0 0)
;;      (0 1 0 0 0 0 0 0)     (1 0 0 0 0 0 0 0)              
;;                                                             
;;      (0 0 1 0 0 0 0 0)     (1 0 0 0 0 0 0 0)               
;;      (0 0 0 1 0 0 0 0)     (0 1 0 0 0 0 0 0)              
;;                                                             
;;      (0 0 0 0 1 0 0 0)     (1 0 0 0 0 0 0 0)     filter    (1 0 0 0 0 0 0 0)
;;      (0 0 0 0 0 1 0 0)     (0 0 1 0 0 0 0 0)     ->        (0 0 0 0 1 0 0 0) 
  ;;                                                            (0 1 0)
;;                                                             
;;      (0 0 0 0 0 0 1 0)     (1 0 0 0 0 0 0 0)               (1 0 0 0 0 0 0 0)
;;      (0 0 0 0 0 0 0 1)     (0 0 0 1 0 0 0 0)               (0 0 0 1 0 0 0 0)
;;                                                             
;;                            (1 0 0 0 0 0 0 0)               (1 0 0 0 0 0 0 0)
;;                            (0 0 0 0 1 0 0 0)               (0 0 0 0 1 0 0 0)
;;                                                             
;;                            (1 0 0 0 0 0 0 0)               (1 0 0 0 0 0 0 0)
;;                            (0 0 0 0 0 1 0 0)               (0 0 0 0 0 1 0 0)
;;                                                             
;;                            (1 0 0 0 0 0 0 0)               (1 0 0 0 0 0 0 0)
;;                            (0 0 0 0 0 0 1 0)               (0 0 0 0 0 0 1 0)
;;                                                             
;;                            (1 0 0 0 0 0 0 0)               (1 0 0 0 0 0 0 0)
;;                            (0 0 0 0 0 0 0 1)               (0 0 0 0 0 0 0 1)

;;(append (list (list 1)) (list (list 2)))

(safe? 2 (list (list 1 2) (list 1 2)))

;;(filter (lambda (positions) (safe? 2 positions)) (list (list (list 1 2) (list 2 2))))
;;(gen-seqs-by-cols (list (list 1 2) (list 2 2)))
