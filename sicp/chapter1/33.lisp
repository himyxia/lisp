(define (filtered-accumulate filter term a next b null-value combiner)
    (cond ((> a b) null-value)
		          ((filter (term a)) (combiner (term a) (filtered-accumulate filter term (next a) next b null-value combiner)))
				          (else (filtered-accumulate filter term (next a) next b null-value combiner))))
  
(filtered-accumulate prime? square a incr b 0 +)

(filtered-accumulate relate-prime? identify 1 incr n 1 *)
