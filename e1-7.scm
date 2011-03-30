;; Michael DeWitt
;; 29 March 2011
;; SICP 1.1.7 Example: Square Roots by Newton's Method
;;	Following along with the example in the book for using 
;;	Newton's Method to approximate square roots.  First
;;	(useful) exercise in recursion in this book.

;;
;; Procedure declarations
;;

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
	guess
	(sqrt-iter (improve guess x)
			   x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average a b)
  (/ (+ a b) 2))

(define (good-enough? guess x)
  ; Text-book example only used abs. diff. to judge accuracy.
  ; If computing the sqrt of a value below that constant, the value
  ; itself would be an acceptable answer, which is obviously wrong 
  ; since the min. acceptable difference in the book was less than 
  ; one.  Percent error is better, but a context-aware delta value
  ; would be better still.  As the guess approaches the actual value, 
  ; the difference between the current guess and a new guess will 
  ; approach zero.
  (< (/ (abs (- guess
				(improve guess x)))
		guess)
	 0.00001))

(define (square x)
  (* x x))

(define (sqrt-newton x)
  (sqrt-iter 1.0 x))


;;
;; Test code
;;

(display (sqrt-newton 2)) (newline) (display (square (sqrt-newton 2))) (newline)
(display (sqrt-newton 9)) (newline) (display (square (sqrt-newton 9))) (newline)
(display (sqrt-newton 100000000)) (newline) (display (square (sqrt-newton 100000000))) (newline)
