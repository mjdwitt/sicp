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


;;
;; 1.21:
;;	Use the smallest-divisor procedure to find the smallest
;;	divisor of each of the following numbers: 199, 1999,
;;	and 19999.

(define (output-smallest n)
  (display n) (display "\t")
  (display (smallest-divisor n))
  (newline))

(display "1.21:") (newline)
(output-smallest 199)
(output-smallest 1999)
(output-smallest 19999)

;;
;; 1.22:
;;	The following procedure tests an integer's primality 
;;	and records the amount of time spent on the test.

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (define (report time)
	(display " *** ") (display time))
  (if (prime? n)
	(report (- (runtime) start-time))))



;;
;; tests

(timed-prime-test 1999)
