;; Michael DeWitt
;; 05 May 2011
;; SICP Exercise 1.17:
;;	Define a procedure for computing multiplication using 
;;	repeated addition that operates with a logarithmic
;;	time complexity.

(define (* a b)
  (define (double n)
	(+ n n))
  (define (halve n)
	(/ n 2))
  (cond ((= b 0) 0)
		((odd? b) (+ a (* a (- b 1))))
		(else (* (double a) (halve b)))))

(display (* 2 8)) (newline)
