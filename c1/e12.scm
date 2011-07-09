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
  (define (partial-sum n) (/ (* n (+ n 1)) 2))
  (define (row n)
    (define (iter i n)
      (if (>= (partial-sum i) n)
        i
        (iter (+ i 1) n)))
    (iter 1 n))
  (define (edge? n)
    (or (= n (partial-sum (row n)))
        (= n (- (partial-sum (row n)) (row n) -1))))
  (if (edge? n)
    1
    (+ (P (- n (row n)))
       (P (- n (row n) -1)))))

(display (P 100)) (newline)
