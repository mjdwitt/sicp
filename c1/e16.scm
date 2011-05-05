;; Michael DeWitt
;; 4 May 2011
;; SICP Exercise 1.16:
;;	Design an iterative procedure to calculate exponents
;;	in a logarithmic number of steps.

(define (^ b n)
  (define (odd? n)
	(= (modulo n 2) 1))
  (define (iter a b n)
	(cond ((= n 0) a)
		  ((odd? n) (iter (* a b) b (- n 1)))
		  (else (iter a (* b b) (/ n 2)))))
  (iter 1 b n))

(display (^ 2 100)) (newline)
