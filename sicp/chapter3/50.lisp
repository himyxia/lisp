;; cannnot figure it out
;; input: a list of streams
;; output: a list of streams (proc mapped)

(define (stream-map proc stream)
  (if (stream-null? stream)
	the-empty-stream
	(cons-stream (proc (stream-car stream))
				 (stream-map proc (stream-cdr stream)))))


(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
	the-empty-stream
	(cons-stream
		(apply proc (map stream-car argstreams))
		(apply stream-map 
			   (cons proc (map stream-cdr argstreams))))))
