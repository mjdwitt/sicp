;; Michael DeWitt
;; 05 May 2011
;; SICP Exercise 1.19

(define (fib n)
  (define (even? x)
	(= (modulo x 2) 0))
  (define (iter a b n)
	(cond ((= n 0) b)
		  ((even? n) (iter a
						   b
						   ;p'
						   ;q'
						   (/ n 2)))
		  (else (iter (+ (* b q) (* a q) (* a p))
					  (+ (* b p) (* a q))
					  p
					  q
					  (- n 1)))))
  (iter 1 0 0 1 n))

(display (fib 9)) (newline)
(display (fib 100)) (newline)
