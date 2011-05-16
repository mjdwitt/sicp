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
;;	If you are using the MIT/GNU Scheme REPL, you can use the
;;	(runtime) primitive to time a procedure.  (If, like most
;;	people, you are using an alternative implementation, none
;;	seem to use this primitive for reasons that I cannot seem
;;	to figure out.  In that case, you can wrap the expression
;;	in a (time <expr>) expression, although that is much more
;;	verbose.)  Using the (runtime) primitive, however, you can
;;	wrap any expression in a procedure (such as the one below)
;;	in which you measure the initial runtime and find the 
;;	difference when the procedure completes.

(define (timed-prime? n)
  ; The timed-prime-test procedure as in SICP 1.22 except with sub-
  ; processes defined nested inside for clarity.
  (define (start n time1)
    (define (report delta-time)
      ; reports runtime and returns true
      (display " *** ")
      (display delta-time)
      #t)
    ; modified from text to return boolean values indicating the
    ; primality of n
    (if (prime? n)
      (report (- (runtime) time1))
      #f))
  (newline)
  (display n)
  (start n (runtime)))

;;	Using this procedure, write a procedure for finding the next
;;	three primes following a given whole number.

(define (search-for-primes n)
  ; checks the primality of consecutive odd integers following n
  ; until three primes have been found
  (define (iter n count)
    ; loops until three primes have been found (count = 3).  Assumes
    ; count starts as 0 and n is already odd.
    (if (< count 3) (iter (+ n 2)
                          (+ count (if (timed-prime? n) 1 0)))
                    "done"))
  (if (= (modulo n 2) 0) (iter (+ n 1) 0)
                         (iter n 0)))
