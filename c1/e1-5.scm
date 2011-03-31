;; Michael DeWitt
;; 28 March 2011
;; SICP Exercise 1.5:
;;	Ben Bitdiddle has invented a test to determine whether 
;;	the interpreter he is faced with is using applicative-
;;	order evaluation or normal-order evaluation. He defines 
;;	the following two procedures:

(define (p) (p))

(define (test x y)
  (if (= x 0) 0 y))

;;	Then he evaluates the expression

(test 0 (p))

;;	What behavior will Ben observe with an interpreter that 
;;	uses applicative-order evaluation? What behavior will 
;;	he observe with an interpreter that uses normal-order 
;;	evaluation? Explain your answer. (Assume that the 
;;	evaluation rule for the special form if is the same 
;;	whether the interpreter is using normal or applicative 
;;	order: The predicate expression is evaluated first, and 
;;	the result determines whether to evaluate the consequent 
;;	or the alternative expression.)

;; Answer:
;;	An interpreter using applicative-order evaluation will 
;;	hang, caught in the infinitely recursive body of (p). 
;;  This is due to appl-order's method of evaluating the 
;;  parameter expressions in each function call before 
;;  evaluating the body of the respective function.  A 
;;  normal-order interpreter would not hang since it only 
;;  evaluates each parameter as needed.  Since the first
;;  parameter expression passed in line 16 is 0, the second
;;  expression--(p)--will never be needed, and consequently,
;;  never be evaluated.

;;  According to this test, the interpreter I am using--
;;  gamibtc--is an applicative-order interpreter, like most 
;;  Scheme interpreters.
