;; Michael DeWitt
;; 03 July 2011
;; SICP Exercise 1.46:
;;	Write a procedure iter-improve that takes two procedures
;;	as arguments: a method for telling whether a guess is good
;;	enough, and a method for improving a guess.  iter-improve
;;	should return a procedure that takes a guess argument and
;;	keeps improving the guess until it is good enough.

(define (iter-improve done? improve)
  (lambda (guess)
	(if (done? guess)
	  guess
	  ((iter-improve done? improve) (improve guess)))))



;;	Rewrite sqrt using the above iter-improve.

(define (sqrti x)
  ((iter-improve
	 (lambda (g)
	   (< (abs (- (square g) x)) .00001))
	 (lambda (g)
	   (/ (+ g
			 (/ x g))
		  2)))
   1.0))



;;	Also rewrite fixed point to use iter-improve.

(define (fpi f guess)
  ((iter-improve
	 (lambda (g)
	   (< (abs (- g (f g))) .00001))
	 f)
   guess))
