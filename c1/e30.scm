;; Michael DeWitt
;; May 26 2011
;; SICP Exercise 1.30:
;;	Rewrite the sum procedure implemented in this section
;;	as an iterative process.

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ (term a) result))))
  (iter a 0))
