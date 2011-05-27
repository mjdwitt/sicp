;; Michael DeWitt
;; 26 May 2011
;; SICP Exercise 1.31:
;;	Write an abstraction similar to sum which multiplies a
;;	range of terms in a sequence.

(define (product term a next b)
  (if (> a b)
	1
	(* (term a) (product term (next a) next b))))

;;	Use this procedure to compute approximations to pi given
;;	the formula that 
;;		pi/4 = (2/3)(4/3)(4/5)(6/5)(6/7)(8/7)...

(define (approximate-pi n)
  ; Numerically approximates pi using the above formula, repeating
  ; the multiplication to the nth term.
  (define (term x)
	(cond ((= x 2) (/ 2.0 3.0))
		  (else (* (/ x (- x 1)) (/ x (+ x 1))))))
  (define (next x) (+ x 2))
  (* 4 (product term 2 next (+ n 1))))
