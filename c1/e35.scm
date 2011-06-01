;; Michael DeWitt
;; 30 May 2011
;; SICP Section 1.3.3: Procedures as General Methods

;; 
;; example code

(define tolerance 0.00001)  ; default tolerance to use with fixed-point's close-enough?

(define (fixed-point f x)
  ; used fixed-point approximation to solve for x in f
  (define (close-enough? a b)
    ; determines whether two consecutive approximations are
    ; close enough in value to be considered approximately
    ; equal
    (< (abs ( - a b)) tolerance))
  (define (iter x)
    ;  iteratively finds the solution for x
    (define next (f x))
    (if (close-enough? x next)
      next
      (iter next)))
  (iter x))



(newline)
(newline) (display "1.35:") (newline)
;;	Find the golden ratio using the above fixed-point procedure 
;;	on the transformation of x -> 1 + 1/x

; the actual formula for the golden ratio, as defined in 1.2.2
(define golden-ratio (/ (+ 1 (sqrt 5)) 2))

; and a fixed-point approximation
(define golden-ratio-fp
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
			   1.0))

(display "Actual ratio:\t") (display golden-ratio) (newline)
(display "Approximation:\t") (display golden-ratio-fp) (newline)
(display "Difference:\t") (display (abs (- golden-ratio golden-ratio-fp))) (newline)



