(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) (list) sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))








(define (map p sequence)
  (accumlator (lambda (x y) (cons (p x) y)) (list) sequence))

(map square (list 1 2 3 4 5))

(define (append seq1 seq2)
  (accumlator cons seq2 seq1))

 (append (list 1 2 3 4) (list 5 6 7 8)) 
 
(define (length seq)
  (accumlator (lambda (x y) (+ 1 y)) 0 seq))
