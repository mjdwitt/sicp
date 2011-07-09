;; Michael DeWitt
;; 26 May 2011
;; SICP Exercise 1.31:
;;	A.) Write an abstraction similar to sum which multiplies a
;;	range of terms in a sequence.

(define (product term a next b)
  (if (> a b)
    1
    (* (term a) (product term (next a) next b))))

;;	Use this procedure to compute approximations to pi given
;;	the formula that 
;;		pi/4 = (2/3)(4/3)(4/5)(6/5)(6/7)(8/7)...

(define (approximate-pi n f)
  ; Numerically approximates pi using the above formula, repeating
  ; the multiplication to the nth term.
  (define (term x)
    (cond ((= x 2) (/ 2.0 3.0))
          (else (* (/ x (- x 1)) (/ x (+ x 1))))))
  (define (next x) (+ x 2))
  (* 4 (f term 2 next (/ n 2))))

;;	B.) Since the above implementation of product is recursive,
;;	write a version that is iterative.

(define (iter-product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* (term a) result))))
  (iter a 1))

;; 
;; some side-by-side testing of the two procedures using approximate-pi

(define (print-pi n)
  ; prints the output of approximate-pi when using both the 
  ; iterative and recursive methods
  (newline)
  (display n) (display "\t")
  (display (approximate-pi n product)) (display "\t")
  (display (approximate-pi n iter-product)))

(print-pi 100)
(print-pi 1000)
(print-pi 10000)
