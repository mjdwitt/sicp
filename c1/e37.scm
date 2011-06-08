;; Michael DeWitt
;; 07 July 2011
;; SICP Exercise 1.37:
;;	Write a function for computing continued fractions 
;;	truncated to the kth term.

(define (cont-frac N D k)
  ; computes the k-term finite continued fraction of
  ; N1/(D1 + N2/(D2 + N3/(... Nk/Dk))), where N and 
  ; D are functions of a single argument.  The single
  ; argument for each successive iteration will simply
  ; be the sequence of whole numbers.
  (define (iter i result)
	; iterates backwards from the (k-1)th term, tracking 
	; the current term with i and returning result. 
	; Assumes that i starts positive and result starts 
	; as the value of the kth term.
	(if (< i 1) result
				(iter (- i 1) (/ (N i) (+ (D i) result)))))
  (iter (- k 1) (/ (N k) (D k))))



;;	Now, use this procedure to compute 1/phi, where phi is
;;	the golden ratio--(1 + sqrt 5)/2.

(display "1/phi:\t")
(display (cont-frac (lambda (x) 1.0)
					(lambda (x) 1.0)
					11)) (display "\t")
(display (/ 2 (+ 1 (sqrt 5)))) (newline)

;;	Computing the inverse of the golden ration as an 11th-
;;	term continued fraction returns a result accurate to only
;;	four digits.
