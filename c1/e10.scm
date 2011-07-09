;; Michael DeWitt
;; 25 April 2011
;; SICP Exercise 1.10:
;;	The following procedure defines a mathematical function
;;	called Ackermann's function.

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;;	By this definition following statements evaluate as following:
;;	(A 1 10)
;;	1024 (2^10)
;;	(A 2 4)
;;	65536 (2^2^2^2)
;;	(A 3 3)
;;	65536

(define (f n)
  ; uses Ackermann's function to compute 2n
  (A 0 n))

(define (g n)
  ; same to compute n^2
  (A 1 n))

(define (h n)
  ; same to computer 2 tetrated to the n
  (A 2 n))
