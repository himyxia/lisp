(define rand
  (let ((x random-init))
	(lambda ()
	  (set! x (rand0-update x))
	  x)))


