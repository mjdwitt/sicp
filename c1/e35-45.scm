;; Michael DeWitt
;; 30 May 2011
;; SICP Section 1.3.3: Procedures as General Methods

;; 
;; example code

(define (average a b)
  (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x)
    (average x
             (f x))))

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
    ;(display x) (newline) ; some tracking output for 1.36
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


(newline)
(newline) (display "1.36:") (newline)
;;	Modify fixed-point to print the current value of x
;;	on each step.  Using this modified procedure, solve for
;;	x in 
;;
;;			x = log(1000)/log(x)
;;	
;;	Compare how average damping affeects the number of steps.

(display "With damping:\t")
(define x1 
  (fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2))
               2.0))
(display x1) (newline)

(display "And without:\t")
(define x2
  (fixed-point (lambda (x) (/ (log 1000) (log x)))
               2.0))
(display x2) (newline)



;; Michael DeWitt
;; 07 July 2011
;; SICP Exercises 1.37-38



(newline) (display "1.37:") (newline)
;;	A.)	Write a function for computing continued fractions 
;;		truncated to the kth term.

(define (cont-frac N D k)
  ; computes the k-term finite continued fraction of
  ; N1/(D1 + N2/(D2 + N3/(... Nk/Dk))), where N and 
  ; D are functions of a single argument.  The single
  ; argument for each successive iteration will simply
  ; be the sequence of whole numbers.
  (define (iter i result)
    ; iterates backwards from the (k-1)th term, tracking 
    ; the current term with i and returning result. 
    ; Assumes that i starts positive and result starts 
    ; as the value of the kth term.
    (if (= i 0) result
                (iter (- i 1) (/ (N i) (+ (D i) result)))))
  (iter k 0))

;;		Now, use this procedure to compute 1/phi, where phi is
;;		the golden ratio--(1 + sqrt 5)/2.

(display "1/phi:\t")
(display (cont-frac (lambda (x) 1.0)
                    (lambda (x) 1.0)
                    11)) (display "\t")
(display (/ 2 (+ 1 (sqrt 5)))) (newline)

;;		Computing the inverse of the golden ration as an 11th-
;;		term continued fraction returns a result accurate to only
;;		four digits.



;;	B.)	Implement and test a recursive version of the above 
;;		procedure for computing continued fractions.


;;		The recursive version:
(define (rcont-frac N D k)
  (define (recurse i)
    (if (> i k)
      0
      (/ (N i) (+ (D i) (recurse (+ i 1))))))
  (recurse 1))

;;		And the test:
(display "\t")
(display (rcont-frac (lambda (x) 1.0)
                     (lambda (x) 1.0)
                     11)) (display "\t")
(display (/ 2 (+ 1 (sqrt 5)))) (newline)



(newline)
(newline) (display "1.38:") (newline)
;;	Write a procedure for approximating e using Euler's 
;;	expansion via one of the above continued fraction
;;	functions.

(define (approx-e k)
  ; Aprroximates e according to Euler's Expansion to 
  ; the kth term.
  (define (N i) 1.0)
  (define (D i)
    (if (= (modulo i 3) 2)
      (* 2 (+ (floor (/ i 3)) 1))
      1))
  (+ 2 (cont-frac N D k)))

(display "e:\t") (display (approx-e 50)) (newline)



(newline)
(newline) (display "1.39") (newline)
;;	Write a continued fraction representation of the tangent
;;	function using J. H. Lambert's approximation.

(define (tan-cf x k)
  ; Computes tangent using Lambert's approximation to the 
  ; kth term.
  (define (N i)
    (if (= i 1) x (- (square x))))
  (define (D i)
    (- (* 2.0 i) 1))
  (cont-frac N D k))

(display "tan(42) = ")
(display (tan-cf 42 100)) (newline)


(newline)
(newline) (display "1.40:") (newline)
;;	Define a procedure cubic that can be used together with 
;;	newton's method to approximate zeros of the cubic
;;		x^3 + ax^2 + bx + c

(define (newtons-method g guess)
  ; code for Newton's method from the text
  (define dx .00001)
  (define (deriv g)
    ; approximates a derivative according to the hardcoded
    ; dx as set above
    (lambda (x)
      (/ (- (g (+ x dx)) (g x))
         dx)))
  (define (newton-transform g)
    (lambda (x)
      (- x (/ (g x) ((deriv g) x)))))
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  ; returns a cubic polynomial function in terms of x in
  ; the form of
  ;        x^3 + ax^2 + bx + c
  (lambda (x)
    (+ (expt x 3)
       (* a (expt x 2))
       (* b x)
       c)))

(display "The polynomial x^3 + x^2 + 2x + 3 has a zero at x = ")
(display (newtons-method (cubic 1 2 3) 1)) (newline)



(newline)
(newline) (display "1.41:") (newline)
;;	Define a procedure double that takes a procedure of one 
;;	argument as an argument and returns a procedure that 
;;	applies the original procedure twice.

(define (double f)
  (lambda (x)
    (f (f x))))

;;	What does (((double (double double)) inc) 5) return?

(define (inc x)
  ; have to define inc really quick here...
  (+ x 1))

(display "(((double (double double)) inc) 5) returns ")
(display (((double (double double)) inc) 5))
(display ".") (newline)



;; Exercise 1.42:
;;	Write a procedure which takes two functions as parameters
;;	and returns a composition of the two.

(define (compose f g)
  (lambda (x)
    (f (g x))))



;; Exercise 1.43:
;;	Write a procedure that takes as inputs a procedure that 
;;	computes f and a positive integer n and returns the 
;;	procedure that computes the nth repeated application of f.

(define (repeated f n)
  (if (= n 1)
    f
    (compose f (repeated f (- n 1)))))



(newline)
(newline) (display "1.43:") (newline)
;;	Write a procedure which returns a smoothed version of any 
;;	single-parameter function passed in to it.

(define (smooth f)
  (define dx .00001)
  (lambda (x)
    (average (f (- x dx))
             (f (+ x dx)))))

;;	Demonstrate how to generate the n-fold smoothed function of
;;	any function using smooth and repeated.

(display "tan(42):\t\t\t") (display (tan 42)) (newline)
(display "smoothed tan(42):\t\t") (display ((smooth tan) 42)) (newline)
(display "42-fold smoothed tan(42:\t") (display ((repeated (smooth tan) 42) 42)) (newline)



(newline)
(newline) (display "1.45:") (newline)
;;	Implement a simple procedure for computing nth roots using
;;	fixed-point, average-damp, and the repeated procedure from
;;	exercise 1.43.

(define (n-rt x p)
  (fixed-point ((repeated average-damp
                           (floor (/ (log p) (log 2))))
                 (lambda (y) (/ x (expt y (- p 1)))))
                1.0))
