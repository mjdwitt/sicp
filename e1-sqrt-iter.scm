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
  (< (abs (- (square guess) x)) 0.001))

(define (square x)
  (* x x))

(define (sqrt-newton x)
  (sqrt-iter 1.0 x))


;;
;; Test code
;;

(display (sqrt-newton 2)) (newline)
(display (sqrt-newton 9)) (newline)
(display (sqrt-newton 100)) (newline)
