; phone code
; 4.7.2011

(define (iter-improve done? improve)
  (lambda (guess)
	(if (done? guess)
	  guess
	  ((iter-improve done? improve) (improve guess)))))

(define (sqrti x)
  ((iter-improve
	 (lambda (g)
	   (< (abs (- (square g) x)) .00001))
	 (lambda (g)
	   (/ (+ g
			 (/ x g))
		  2)))
   1.0))

(define (fpi f guess)
  ((iter-improve
	 (lambda (g)
	   (< (abs (- g (f g))) .00001))
	 f)
   guess))
