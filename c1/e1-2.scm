;; SICP Excercise 1.2
;; Michael DeWitt
;; 26 March 2011

(define val
  (/ (+ 5 4 (- 2
			   (- 3
				 (+ 6 (/ 4 5)))))
	 (* 3 (- 6 2) (- 2 7))))

(display val) (newline)
