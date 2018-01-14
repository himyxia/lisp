;; think, why this one doesn't work
(define (fringe tree)
	(if (null? tree)
		tree
		(let ((first (car tree)))
		(append ((if (pair? first)
					(fringe first)
					(list first))
				(fringe (cdr tree)))))))

