(define (factorial x)
  (cond ((or (= x 0) (= x 1)) 1)
		((> x 1) (* x (factorial (- x 1))))))


