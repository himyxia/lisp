(define a (make-wire))
(define b (make-wire))
(define c (make-wire))

(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

(or-gate a b d)

(and-gate a b c)

(inverter c e)

(and-gate d e s)

(define (probe name wire)
  (add-action! wire
			   (lambda()
				 (newline)
				 (display name)
				 (display " ")
				 (display (current-time the-agenda))
				 (display " New-value = ")
				 (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 3)


(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(prob 'sum sum)

(prob 'carry carry)

(half-adder input-1 input-2 sum carry)

(set-signal! input-1 1)

(propagate)

(set-signal! input-2 1)

(propagate)
