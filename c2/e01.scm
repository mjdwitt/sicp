;; Michael DeWitt
;; 09 July 2011
;; SICP Exercise 2.1:
;;	Define a better version of make-rat that handles both 
;;	positive and negative arguments.  Make-rat should 
;;	normalize the sign so that if the rational number is 
;;	positive, both the numerator and denominator are positive,
;;	and if the number is negative, only the numerator
;;	is negative.

(define (make-rat n d)
  (let ((g (gcd n d))
		(n (if (< d 0) (- n) n))
		(d (if (< d 0) (- d) d)))
	(cons (/ n g) (/ d g))))

;; example code

(define (numer rat)
  (car rat))

(define (denom rat)
  (cdr rat))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
			   (* (numer y) (denom x)))
			(* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
			   (* (numer y) (denom x)))
			(* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
			(* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
			(* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
	 (* (numer y) (denom x))))
