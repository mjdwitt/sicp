(define (factorial x)
  (cond ((= x 1) 1)
		((> x 1) (* x (factorial (- x 1))))))
