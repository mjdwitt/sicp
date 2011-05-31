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




