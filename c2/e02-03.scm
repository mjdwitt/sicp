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

(define (lenght-line line) ; *
  (distance (start-segment line)
	    (end-segment line)))

(define (slope-line line) ; *
  (/ (- (x-point (end-segment line))
	(x-point (start-segment line)))
     (- (y-point (end-segment line))
	(y-point (start-segment line)))))

; * denotes functions not truly needed for 2.2, but come in 
; handy later.



;; Exercise 2.3:
;;	Implement a representation for rectangles in a plane.

(define (make-rect line width)
  ; make-rect defines a rectangle in terms of one of its edges
  ; and the distance between the given edge and the parallel edge.
  ; Given that line and scalar, we can compute each of the four
  ; vertexes using the functions for lines and points defined above.
  (define skew (slope-line line)) ; the rectangle's skew, relative to the x-axis
  ; The induvidual components of the start and endpoint of the given edge:
  (define xa (x-point (start-segment line)))
  (define ya (y-point (start-segment line)))
  (define xb (x-point (start-segment line)))
  (define yb (y-point (start-segment line)))
  ; The individual components of the rectangle's remaning 
  ; two vertices, defined in terms of the two which are given:
  (define xc (+ (/ (* (sqrt (+ (expt skew -2)
			       1))
		      width)
		   (+ (expt skew -2)
		      1))
		xb))
  (define yc (+ (* (expt skew -1)
		   (- xc xb))
		yb))
  (define xd (/ (+ (* (expt skew 2)
		      xc)
		   (- (* skew yc))
		   (* skew ya)
		   (- xa))
		(- (expt skew 2)
		   1)))
  (define yd (+ (* skew
		   (- xd xc))
		yc))

  ; The definitions of the four vertices:
  (define A (start-segment line))
  (define B (end-segment line))
  (define C (make-point xc yc))
  (define D (make-point xd yd))

  ; The rectangle is stored as a collection of four points--
  ; or more acurately, a pair of two pairs of points:
  ; ((A, B), (C, D))
  (cons (cons A B) (cons C D)))

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
  (cond ((string-ci=? letter "A") (car (car rect)))
	((string-ci=? letter "B") (cdr (car rect)))
	((string-ci=? letter "C") (car (cdr rect)))
	((string-ci=? letter "D") (cdr (cdr rect)))))

; The edges can each be accessed by the same function 
; according to their numbers, where side 1 is the AB segment,
; 2 is BC, 3 is CD, and 4 is DA.
(define (rect-edge rect num) 42)

;;	In terms of your constructors and selectors, create 
;;	procedures that compute the perimeter and the area of
;;	a given rectangle.


