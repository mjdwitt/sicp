;; Michael DeWitt
;; 26 May 2011
;; SICP Exercise 1.29

;; 
;; example code:

(define (cube x) (* x x x))    ; raises x to the third power

(define (sum term a next b)
  ; An implementation of a summation series, as would be 
  ; computed using sigma notation.  Arguments term and
  ; next are procedures respectively defining the general
  ; form of each term in the series and the method used
  ; to increment from one term to the next.  a fills the 
  ; role of i in sigma notation with b being the end
  ; value for the process.
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (integral f a b dx)
  ; Finds the integral of f from a to b using dx as the 
  ; width of each sub-area.
  (define (add-dx x) (+ x dx))
  (* dx (sum f (+ a (/ dx 2.0)) add-dx b)))



(newline)
(display "1.29:") (newline)
;;	Implement a numerical method for computing integrals 
;;	using Simpson's Rule and the above summation procedure.

(define (simpsons-rule f a b n)
  ; Numerically computes integrals.
  ; f is a mathematical function in terms of a single 
  ; variable.  The procedure is applied over the range
  ; of [a, b] in n steps. (n is assumed to be even.)
  (define h (/ (- b a) n))    ; defines the width of each section being summed
  (define (next x) (+ x h ))        ; finds the next value of x for the next term of simpson's rule
  (define (term x)                    ; maps x to the correct form for its term in simpson's rule
    (define (even-term? x)
      ; Determines if the value of x represents an even
      ; term in the series used by simpson's rule.  This
      ; could be easily modified to determine if the sub-
      ; interval implied by x is an even or odd ordered 
      ; interval in any interval from a to b.
      (= (modulo (/ (* (- x a)
                       n)
                    (- b a))
                 2)
         0))
    (cond ((or (= x a) (= x b)) (f x))    ; the first and last terms
          ((even-term? x) (* 4 (f x)))    ; even terms all get multiplied by 4
          (else (* 2 (f x)))))            ; odd terms get multiplied by 2
  (* (/ h 3.0) (sum term a next b)))

;;	Use the above implementation to integrate cube between 
;;	0 and 1 with n set at 100 and 1000.

(define (test-sr f a b n)
  ; a procedure for outputing information returned by
  ; simpsons-rule.
  (newline)
  (display n) (display "\t")
  (display (simpsons-rule f a b n)))

(test-sr cube 0 1 100)
(test-sr cube 0 1 1000)
(newline)




