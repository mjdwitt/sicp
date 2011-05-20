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
                  ((= start 2) (iter n (+ start 1)))
		  (else (iter n (+ start 2)))))
  (iter n 2))

(define (prime? n)
  ; returns true if no divisor other than self is found
  (= n (smallest-divisor n)))



;; Fermat test

(define (expmod b x m)
  (cond ((= x 0) 1)
        ((= (modulo x 2) 0) (modulo (square (expmod b (/ x 2) m))
                                    m))
        (else              (modulo (* b (expmod b (- x 1) m))
                            m))))

(define (fermat-test n)
  (define (try a)
    (= (expmod a n n) a))
  (try (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))



(newline) (display "1.21:") (newline)
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
(newline)
(newline) (display "1.22:") (newline)
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
    (if (fast-prime? n 4)
      (report (- (process-time-clock) time1))
      #f))
  (newline)
  (display n)
  (start n (process-time-clock)))

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
  (newline) (display n) (display "---------------------------")
  (if (= (modulo n 2) 0) (iter (+ n 1) 0)
                         (iter n 0))
  (newline) (display "-----------------------------------"))

(search-for-primes 1000)
(search-for-primes 10000)
(search-for-primes 100000)
(search-for-primes 1000000)

;;	Given that (runtime) only measures system time in seconds, I 
;;	rewrote the above to use the mit-scheme primitive (process-
;;	time-clock), which measures in ticks.  Since ticks are
;;	currently defined as only milliseconds, they still lack the
;;	necessary resolution to form a conclusion only using 100,000
;;	and 1,000,000 as values for n in search-for-primes.  Perhaps 
;;	larger values of n will work?  Let's try.

(search-for-primes 1000000000000000000000000)
(search-for-primes 1000000000000000000000000000000000000000000000000)

;;	Using larger values for n and the (process-time-clock) primitive,
;;	it becomes clear that (prime? <val>) has a time complexity of
;;	O(sqrt(n).



;; 1.23:
;;	Modify (smallest-divisor <val>) to skip testing all even values
;;	above 2.  While this did not quite halve the runtime for 
;;	(prime? <val>), it came close.  The small difference is most likely
;;	due to some static (non-repeating) code, either in the present code
;;	in the Scheme REPL itself.



;; 1.24:
;;	Modify the (timed-prime? <val>) procedure to use the Fermat method
;;	and compare the runtimes for the values tested earlier in 1.22. 
;;	While this certainly makes it faster, even 10^24 and 10^48 are too
;;	small of values for timed-prime? to return any meaningful results.
;;	Other solutions for this and the previous timed-prime? exercises
;;	all seem to obtain meaningful results from runtime, so I am beginning
;;	to wonder if there is some problem with the installation of 
;;	mit-scheme that I am running on this system.  I'll have to try it
;;	on my desktop when I finish this trip.
