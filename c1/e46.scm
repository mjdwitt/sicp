; phone code
; 4.7.2011

(define (iter-improve done? improve)
  (lambda (guess)
	(if (done? guess)
	  guess
	  ((iter-improve done? improve) (improve guess)))))


