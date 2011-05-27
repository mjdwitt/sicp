;; Michael DeWitt
;; 27 May 2011
;; SICP Exercise 1.33:
;;	Implement a version of accumulate from 1.32 which uses
;;	a filter procedure provided as an additional parameter
;;	to select which of the values within the set specified
;;	by a, b, and next to include in the collection returned.

(define (filtered-accumulate include? combiner null-value term a next b)
  (cond ((> a b) null-value)
		((include? (term a)) (combiner (term a) 
									   (filtered-accumulate include?
															combiner
															null-value
															term
															(next a)
															next
															b)))
		(else (combiner null-value
						(filtered-accumulate include?
											 combiner
											 null-value
											 term
											 (next a)
											 next
											 b)))))

;; some quick tests for filtered-accumulate:

(define (id x) x)
(define (inc x) (+ x 1))
(define (use? x) true)
(define (test f z n)
  (filtered-accumulate use? f z id 1 inc n))

(display (test + 0 10)) (newline)
(display (test * 1 5)) (newline)

;;	Show how to express the following using the above code:
;;	A.)	the sum of the squares of the prime numbers in ther interval a to b,

;;	and B.)	the product of all positive integers i < n such that GCD(i,n) = 1.
