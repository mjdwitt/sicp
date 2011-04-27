;; Michael DeWitt
;; 26 March 2011
;; SICP Excercise 1.4:
;;		Observe that our model of evaluation allows for combinations whose
;;		operators are compound expressions. Use this observation to describe
;;		the behavior of the following procedure:

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; Explanation:
;;	The above procedure adds a to to the absolute value of b.  The body syntax
;;	exemplifies the nature of operators (and procedures) as data in Lisp 
;;	philosophy--because simple math operators are just a special value, the 
;;	if statement can return them just as it would any other value.  Because
;;	Scheme model of evaluation allows for compund expressions as operators,
;;	this method can be used to select on operator for the body expression
;;	based on the value of the parameters.
