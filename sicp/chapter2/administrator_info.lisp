(define (get-record name personnel-file)
  (let ((division-name (division-tag personnel-file)))
	  ((get division-name 'get-record) (contents personnel-file) name)))
 
(define (division-tag datum)
  (if (pair? datum)
	(car datum)
	(error "bad datum")))

(define (content datum)
  (if (pair? datum)
	(cdr datum)
	(error "bad datum")))

(define (install-division-1-package)
	(define (make-personnel-file division records) 
	  (cons division records))

	(define (division personnel-file)
	  (car personnel-file))

	(define (records personnel-file)
	  (cdr personnel-file))

	(define (get-record records name)
	  (find-record-by-name name records))

	(put 'division-1 'get-record get-record)
	(put 'division-1 'make-personnel-file make-personnel-file))

(define (get-salary name peronnel-file)
  (let ((record (get-record name personnel-file)))
	(pick 'salary record)))

(define (find-employee-record name . personnel-files)
  (let ((division-tags (map division-tag personnel-files)))
	(let ((proc (get 'get-record division-tags)))
	  (if proc
		(apply proc (map content personnel-files))
		(error "no method for that type")))))

(division-tag record)

;; -> personal file
;;      -> name : record(salary:xxx, address:xxx)
