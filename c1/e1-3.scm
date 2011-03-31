;; SICP Excercise 1.3
;;		"Define a procedure that takes three numbers
;;		as arguments and returns the sum of the squares
;;		of the two larger numbers."
;; Michael DeWitt
;; 26 March 2011

;;
;; Procedure definitions
;;

(define (sqr a)
  ; squares a
  (* a a))

(define (sumsqr a b c)
  ; sums the squares of three numbers
  (+ (sqr a) (sqr b) (sqr c)))

(define (min-of-3 a b c)
  ; returns the smallest of three values
  (cond ((and (< a b) (< a c)) a)
		((= a b) a)
		((and (< b a) (< b c)) b)
		(else c)))

(define (sumsqr-two-large a b c)
  ; returns the sum of the squares of the two larger values
  (- (sumsqr a b c)
	 (sqr (min-of-3 a b c))))

;;
;; Test code
;;

(define val (sumsqr-two-large 4 2 3))
(display val) (newline)
