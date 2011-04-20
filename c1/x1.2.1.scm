;; SICP Example from Fig. 1.3, Section 1.2.1
;; Michael DeWitt
;; 20 Aprile 2011

(define (factorial x)
  (cond ((or (= x 0) (= x 1)) 1)
		((> x 1) (* x (factorial (- x 1))))))
