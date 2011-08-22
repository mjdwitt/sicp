;; Michael DeWitt
;; 09 July 2011
;; SICP Exercises 2.2-3

;; Exercise 2.2:
;;	Define a set of procedures for creating lines and
;;	points.  Using proper abstraction, use these procedures
;;	to create a (midpoint <line>) procedure which returns
;;	the midpoint of a given line.

(define (make-point x y)
  (cons (* 1.0 x) (* 1.0 y)))	; The coordinates are multiplied
  				; here to make all math using points
				; much simpler later.
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))

(define (distance p q) ; *
  (sqrt (+ (expt (- (x-point q)
		    (x-point p))
		 2)
	   (expt (- (y-point q)
		    (y-point p))
		 2))))

(define (print-point p)
  (newline)
  (display "(") (display (x-point p)) (display ",")
  (display (y-point p)) (display ")"))

(define (make-line start end)
  (cons start end))
(define (start-segment line)
  (car line))
(define (end-segment line)
  (cdr line))

(define (midpoint line) 
  (let ((x1 (x-point (start-segment line)))
	(y1 (y-point (start-segment line)))
	(x2 (x-point (end-segment line)))
	(y2 (y-point (end-segment line))))
    (make-point (/ (+ x1 x2) 2)
		(/ (+ y1 y2) 2))))

(define (length-line line) ; *
  (distance (start-segment line)
	    (end-segment line)))

(define (slope-line line) ; *
  ; returns the slope as a ratio for all normal slopes and
  ; #f for undefined (infinite) slopes
  (if (= (x-point (start-segment line))
	 (x-point (end-segment line)))
    #f
    (/ (- (y-point (end-segment line))
	  (y-point (start-segment line)))
       (- (x-point (end-segment line))
	  (x-point (start-segment line))))))

(define (perpendicular-slope-line line) ; *
  ; returns the slope of any line perpendicular to the given line
  (let ((s (slope-line line)))
    (if s
      (/ 1 (- s)) ; s is normal
      0))) ; s is undefined (infinite)

(define (flip-line line)
  ; returns a line whose endpoints are in the opposite order
  (make-line (end-segment line)
	     (start-segment line)))

; * denotes functions not truly needed for 2.2, but come in 
; handy later.



;; Exercise 2.3:
;;	Implement a representation for rectangles in a plane.



; Given the above definition of a rectangle, the below accessors
; all assume that the rectangle is modeled similarly to the
; following:
;
;	A		 B
;	 +--------------+
;	 |		|
;	 |		|
;	 +--------------+
;	D		 C
;
; where the line AB is the line passed to the original call
; to make-rect and the distance between AB and DC is equal
; to the width argument.

; The vertexes can each be accessed by the same function 
; according to their corresponding letters as drawn above:
(define (rect-vertex rect letter)
  (cond ((string-ci=? letter "A") (first rect))
	((string-ci=? letter "B") (second rect))
	((string-ci=? letter "C") (third rect))
	((string-ci=? letter "D") (fourth rect))))

; The edges can each be accessed by the same function 
; according to their numbers, where side 1 is the AB segment,
; 2 is BC, 3 is CD, and 4 is DA.
(define (rect-edge rect num) 42)

;;	In terms of your constructors and selectors, create 
;;	procedures that compute the perimeter and the area of
;;	a given rectangle.


