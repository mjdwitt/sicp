;; Michael DeWitt
;; 05 May 2011
;; SICP Exercise 1.17:
;;	Define a procedure for computing multiplication using 
;;	repeated addition that operates with a logarithmic
;;	time complexity.

(define (* a b)
  (define (double n)
    (+ n n))
  (define (halve n)
    (/ n 2))
  (define (iter a b c)
    (display a) (display "\t") (display b) (display "\t") (display c) (newline)
    (cond ((= b 0) 0)
          ((= b 1) (+ a c))
          ((odd? b) (iter a (- b 1) (+ a c)))
          (else (iter (double a) (halve b) c))))
  (iter a b 0))


(display (* 2 1025)) (newline)
