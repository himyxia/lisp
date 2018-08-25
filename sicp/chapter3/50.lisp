;; cannnot figure it out
;; output: a list of streams
;; output: a list of streams (proc mapped)

(define (stream-map proc stream)
  (if (stream-null? stream)
	the-empty-stream
	(cons-stream (proc (stream-car stream))
				 (stream-map proc (stream-cdr stream)))))


(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
	the-empty-stream
	(begin
		(apply proc (map (lambda (s) (stream-car s)) argstreams))
		(apply stream-map 
			   (cons proc (map (lambda (s) (stream-cdr s)) argstreams))))))
