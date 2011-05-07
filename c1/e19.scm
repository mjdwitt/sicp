;; Michael DeWitt
;; 05 May 2011
;; SICP Exercise 1.19

(define (fib n)
  (define (even? x)
	(= (modulo x 2) 0))
  (define (iter a b p q n)
	(cond ((= n 0) b)
		  ((even? n) (iter a
						   b
						   (+ (* p p) (* q q))
						   (+ (* 2 p q) (* q q))
						   (/ n 2)))
		  (else (iter (+ (* b q) (* a q) (* a p))
					  (+ (* b p) (* a q))
					  p
					  q
					  (- n 1)))))
  (iter 1 0 0 1 n))

(define (print n)
  (cond ((> n 15) (display "done") (newline))
	(else (display (fib n)) (newline)
	      (print (+ n 1)))))

(print 0)
