;; Michael DeWitt
;; 15 May 2011
;; SICP Exercises 1.21 and 22


;; 
;; example code

;; iterative test

(define (smallest-divisor n)
  ; returns the smallest divisor of n
  (define (iter n start)
	; iteratiely finds the smallest divisor of n, 
	; beginning with start
	(cond ((> (* start start) n) n)
		  ((= (modulo n start) 0) start)
		  (else (iter n (+ start 1)))))
  (iter n 2))

(define (prime? n)
  ; returns true if no divisor other than self is found
  (= n (smallest-divisor n)))


(display "1.21:") (newline)
;;	Use the smallest-divisor procedure to find the smallest
;;	divisor of each of the following numbers: 199, 1999,
;;	and 19999.

(define (output-smallest n)
  (display n) (display "\t")
  (display (smallest-divisor n))
  (newline))

(output-smallest 199)
(output-smallest 1999)
(output-smallest 19999)
(newline)



(display "1.22:") (newline)
;;	Forget what the SICP says about (runtime) being a built-in 
;;	Scheme expressoin; it doesn't exist.  Instead, if you care
;;	to find the numbers anyways, use the (time expr) procedure,
;;	where expr is any evaluatable Scheme expression.  i.e.:

(time (prime? 999999999999999999999999999999999999999999999999999999999999999999991))

;;	If you want to output the return value of the expression being 
;;	provided to time, wrap the whole expression in a display
;;	expression, like this:

(display (time (prime? 99999999999999999999999999999999999999999999999999999999999999999991)))



;;
;; tests

