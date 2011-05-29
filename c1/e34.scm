;;Michael DeWitt
;;28 May 2011
;;SICP Exercise 1.34:
;;	Suppose we define the procedure

(define (f g)
  (g 2))

;;	Then we have

(f square)
;4

(f (lambda (z) (* z (+ z 1))))
;6

;;	What happens if we (perversely) ask the interpreter to
;;	evaluate the combination of (f f)?  Explain.

;;	The logic would go a bit like this:
;;		(f f)
;;		(f 2)
;;		(2 2)
;;	At this point, we would encounter an interpreter error
;;	due to the fact that 2 is an integer, not a function.
;;	Let's go ahead and test this hypothesis below:

(f f)
