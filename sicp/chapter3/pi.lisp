(define (estimate-pi trials)
  (sqrt (/ 6 (mote-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand) 1)))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
	(cond ((= trials-remaining 0)))))
