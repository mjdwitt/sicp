;; SICP Example from Fig. 1.3, Section 1.2.1
;; Michael DeWitt
;; 20 Aprile 2011

(define (factorial x)
  (cond ((= x 0) 1)
		((> x 0) (* x (factorial (- x 1))))))
