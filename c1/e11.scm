;; Michael DeWitt
;; 25 April 2011
;; SICP Exercise 1.11:
;;	A function f is defined by the rule that f(n)=n if n<3 and
;;	f(n)=f(n-1)+2f(n-2)+3f(n-3) if n>=3.  Write procedures for
;;	computing f in a recursive process and an iterative one.

(define (fr n)
  ; computes f recursively
  (if (< n 3)
    n
    (+ (fr (- n 1))
       (* 2 (fr (- n 2)))
       (* 3 (fr (- n 3))))))

(define (fi n)
  ; calls the iterative version
  (fiter 2 1 0 n))

(define (fiter a b c n)
  ; computes f iteratively
  (if (= n 0)
    c
    (fiter (+ a (* 2 b) (* 3 c)) a b (- n 1))))
  

(display (fr 6)) (newline)
(display (fi 6)) (newline)
