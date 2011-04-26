;; Michael DeWitt
;; 25 April 2011
;; SICP Exercise 1.12:
;;	The following pattern of numbers is called Pascal's 
;;	Triangle:
;;				1
;;			   1 1
;;			  1 2 1
;;			 1 3 3 1
;;			1 4 6 4 1
;;			   ...
;;	The numbers at the edge of the triangle are all 1 and 
;;	each number inside the triangle is the sum of the two
;;	numbers above it.  Write a procedure that computes 
;;	elements of Pascal's Triangle by means of a recursive
;;	process.

;;
;; Answer:
;;		P(n) = P(n-r) + P(n-r+1),
;;	where r is the row on which the nth term resides.
;;	Additionally,
;;		P(1) = 1, and
;;		P(edge) = 1,
;;	where edge is an element in the subset of all ns
;;	such that edge resides on the edge of the triangle.
;;	This can be expressed as any n such that
;;		edge = SUM of i from i=1 to r, and 
;;		any value of n equal to any of the above edges
;;			plus one.

(define (P n)
  ; recursively computes the value of the nth term in
  ; Pascal's Triangle
  (define (row-iter sum i n)
	; iteratively finds the row
	(if (>= sum n)
	  i
	  (row-iter (+ sum i) (+ i 1) n)))
  ; the number of the row on which the nth term resides
  (define row (row-iter 0 1 n))
  (define (edge? n)
	; identifies terms that are on the edge of the Triangle
	(edge?faster (sigma 1 row 0) n))
  (define (edge?faster row-max n)
	(or (= n row-max) (= n (- row-max row -1))))
  (define (sigma i n sum)
	; finds partial sums of the series of whole numbers
	(if (> i n)
	  sum
	  (sigma (+ i 1) n (+ sum i))))
  (if (edge? n)
	1
	(+ (P (- n row)) (P (- n row -1)))))

(display (P 13)) (newline)
