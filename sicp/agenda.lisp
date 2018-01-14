(define (make-agenda) (list 0))

(define (current-time agenda) (car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda) (car (segments agenda)))

(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
	(delete-queue! q)
	(if (empty-queue? q)
	  (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
	(error "Agenda is empty - FIRST-AGENDA-ITEM")
	(let ((first-seg (first-segment agenda)))
	  (set-current-time! agenda (segment-time first-seq))
	  (front-queue (segment-queue first-seg)))))
