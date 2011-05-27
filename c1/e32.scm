;; Michael DeWitt
;; 26 May 2011
;; SICP Exercise 1.32:
;;	A.)	Show that sum and product are both specific implementations 
;;		of the more abstract concept of accumulation by defining
;;		a general-form function capable of accumulating any
;;		collection of terms using any method of combination.

(define (accumulate combiner null-value term a next b)
  (if (> a b)
	null-value
	(combiner (term a) (accumulate combiner null-value term (next a) next b))))

;;		Demonstrate this function by implementing versions of 
;;		product and sum that use the above in their definitions.

(define (sum f term a next b)
  (f + 0 term a next b))

(define (product f term a next b)
  (f * 1 term a next b))

;;		And some testing for both sum and product:

(define (id x) x)

(define (inc x) (+ x 1))

(display (sum accumulate id 1 inc 10)) (display "\t")
(display (sum iter-accumulate id 1 inc 10)) (newline)
(display (product accumulate id 1 inc 5)) (display "\t")
(display (product iter-accumulate id 1 inc 5)) (newline)

;;	B.)	Since the above implementation of accumulate is a
;;		recursive process, create an iterative version.

(define (iter-accumulate combiner null-value term a next b)
  (define (iter a result)
	(if (> a b)
	  result
	  (iter (next a) (combiner (term a) result))))
  (iter a null-value))

