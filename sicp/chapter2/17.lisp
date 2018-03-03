(define (last-pair list1)
  (cond ((null? list1) (list))
        ((= (length list1) 1) list1)
        (else (last-pair (cdr list1)))))
