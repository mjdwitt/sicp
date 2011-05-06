;; Michael DeWitt
;; 05 May 2011
;; SICP Exercise 1.19

(define (fib n)
  (define (iter a b n)
	(cond ((= n 0) b)
		  (else (iter (+ a b) a (- n 1)))))
  (iter 1 0 n))

(display (fib 9)) (newline)
(display (fib 100)) (newline)
