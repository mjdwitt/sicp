;; Michael DeWitt
;; 29 March 2011
;; SICP 1.1.7 Example: Square Roots by Newton's Method
;;	Following along with the example in the book for using 
;;	Newton's Method to approximate square roots.  First
;;	(useful) exercise in recursion in this book.

;;
;; Procedure declarations
;;

(define (sqrt-newton radicand)
  (sqrt-iter 1.0 radicand))

(define (sqrt-iter guess rad)
  ; main recursive function
  (if (accept? guess rad)
	guess
	(sqrt-iter (improve guess rad)
			   rad)))

(define (improve guess rad)
  ; improves the guess value for the sqrt, according to Newton
  (/ (+ (/ rad
		   guess)
		rad)
	 2))

(define (accept? guess rad)
  ; determines if a guess value is good enough
  (< (/ (abs (- guess 
				rad))
		rad)
	 0.000000001))

;;
;; Test code
;;

(display (sqrt-newton 2)) (newline)
(display (sqrt-newton 9)) (newline)
(display (sqrt-newton 100)) (newline)
