;; Michael DeWitt
;; 09 July 2011
;; SICP Exercises 2.2-3

;; Exercise 2.2:
;;	Define a set of procedures for creating lines and
;;	points.  Using proper abstraction, use these procedures
;;	to create a (midpoint <line>) procedure which returns
;;	the midpoint of a given line.

(define (midpoint line)
  (let ((x1 (x-point (start-segment line)))
	(y1 (y-point (start-segment line)))
	(x2 (x-point (end-segment line)))
	(y2 (y-point (end-segment line))))
    (make-point (/ (+ x1 x2) 2)
		(/ (+ y1 y2) 2))))

(define (make-line start end)
  (cons start end))
(define (start-segment line)
  (car line))
(define (end-segment line)
  (cdr line))

(define (lenght-line line)
  (distance (start-segment line)
	    (end-segment line)))

(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))

(define (distance p q)
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



;; Exercise 2.3:
;;	Implement a representation for rectangles in a plane.

(define (make-rect line width)
  ; defines a rectangle in terms of one of its edges and the
  ; distance between the given edge and its parallel edge.
  (cons line width))

; Given the above definition of a triangle, the below accessors
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


;;	In terms of your constructors and selectors, create 
;;	procedures that compute the perimeter and the area of
;;	a given rectangle.


