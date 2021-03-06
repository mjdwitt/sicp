;; Michael DeWitt
;; 15 May 2011
;; SICP Exercises 1.21 and 22


;; 
;; example code

;; iterative test

(define (prime? n)
 ; returns true if no divisor other than self is found
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

 (= n (smallest-divisor n)))



;; Fermat test

(define (fast-prime? n times)
 (define (fermat-test n)
  (define (expmod b x m)
   (cond ((= x 0) 1)
         ((= (modulo x 2) 0) (modulo (square (expmod b (/ x 2) m))
                                     m))
         (else               (modulo (* b (expmod b (- x 1) m))
                                     m))))
  (define (try a)
   (= (expmod a n n) a))
  (try (+ 1 (random (- n 1)))))
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
        ;;    people, you are using an alternative implementation, none
        ;;    seem to use this primitive for reasons that I cannot seem
        ;;    to figure out.  In that case, you can wrap the expression
        ;;    in a (time <expr>) expression, although that is much more
        ;;    verbose.)  Using the (runtime) primitive, however, you can
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

    ;;    Using this procedure, write a procedure for finding the next
    ;;    three primes following a given whole number.

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



;; 1.25:
;;	Would the fast-expt procedure from 1.2.4 be usable for replacing
;;	large amounts of the code in expmod?  Possibly: a^n (mod m) could
;;	be found by writing
;;
;;		(modulo (fast-expt a n) n)
;;
;;	While the above code would technically find the same answer, it contains
;;	some potential pitfalls not present in the version of expmod as coded
;;	above or in the text, namely, the above code could fail or return 
;;	incorrect answers for extremely large values of n, depending on the
;;	being used to evaluate the code.  By including an evaluation of 
;;	modulo or remainder in each recursive step of expmod, the value being
;;	passed back up the call stack is kept to some number less than n.

;; 1.26
;;	Louis Reasoner's use of explicit multiplication in expmod rather than the 
;;	square procedure causes expmod to become tree-recursive rather than linear
;;	recursive due to the evauation order of most Scheme interpreters.  This
;;	drastically impacts the runtime of fast-prime?.



(newline)
(newline)
(display "1.27:") (newline)
;;	Demonstrate that the Carmichael numbers listed in the text would truly
;;	fool the Fermat method of testing primality by writing a procedure
;;	to test if a is congruent to a^n (mod n) for every a < n.

(define (carmichael? n)
  ; tests to see if n is a carmichael number
  (define (iter a n)
    ; tests all a < n, assuming original call had a = 1
    (cond ((= a n) true)
          ((= (expmod a n n) a) (iter (+ a 1) n))
          (else false)))
  (iter 1 n))

(define (out-car? n)
  ; output function for testing carmichael numbers
  (display n) (display "\t") (display (carmichael? n)) (newline))

;; tests
(out-car? 561)
(out-car? 1105)
(out-car? 1729)
(out-car? 2465)
(out-car? 2821)
(out-car? 6601)

;;	All of the above Carmichael numbers would indeed fool fermat-prime? as
;;	written above.



(newline)
(newline)
(display "1.28") (newline)
;;	The Miller-Rabin test is an unfoolable variation of the Fermat method
;;	of testing for primality.  It states that, for any prime number n and
;;	any whole number a such that a < n, a^(n-1) is congruent to 1 modulo n.
;;	Below is an implementation of this method.

(define (mrprime? n)
  (define (mrexpmod b x m)
    (define (test p q)
        (if (and (not (or (= p 1)
                        (= p (- q 1))))
               (= (expmod p 2 q) 1))
        0
        p))
    (cond ((= x 0) 1)
          ((= (modulo x 2) 0) (modulo (square (test (mrexpmod b (/ x 2) m) m)) m))
          (else (modulo (* b (mrexpmod b (- x 1) m)) m))))
  (define a (+ 1 (random (- n 1))))
  (= (mrexpmod a (- n 1) n) 1))



