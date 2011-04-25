;; Michael DeWitt
;; 25 April 2011
;; SICP Exercise 1.11:
;;	A function f is defined by the rule that f(n)=n if n<3 and
;;	f(n)=f(n-1)+2f(n-2)+3f(n-3) if n>=3.  Write procedures for
;;	computing f in a recursive process and an iterative one.

(define (fr n)
  ; computes f recursively
  (if (< n 3)
	n
	(+ (fr (- n 1))
	   (* 2 (fr (- n 2)))
	   (* 3 (fr (- n 3))))))

(define (fi n)
  ; computes f iteratively
  -1) ; implement

(display (fr 9)) (newline)
