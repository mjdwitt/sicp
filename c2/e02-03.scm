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
;;	Implement two representations for rectangles in a plane.

(define (third-vertex leg distance)
  ; Given a leg of a right triangle and the distance from the 
  ; orthagonal vertext to the third vertex, this procedure returns
  ; the value of the unknown third vertex of the triangle.
  (let* ((p (perpendicular-slope-line leg))
	 (xb (x-point (end-segment leg)))
	 (yb (y-point (end-segment leg)))
	 (x (if p ((if (< 0 distance) + -) xb
					   (sqrt (/ (square distance)
						    (+ 1 (square p)))))
		  xb))
	 (y (if p (+ (* p (- x xb)) yb)
		  (+ yb distance))))
    (make-point x y)))

(define (make-rect-av edge width)
  ; Given one edge and the width between the given edge and
  ; the one parallel, this procedure returns a rectangle 
  ; as a list of four points, where the given edge is the
  ; segment defined by the first two points.
  (let ((A (start-segment edge))
	(B (end-segment edge))
	(C (third-vertex edge width))
	(D (third-vertex (flip-line edge) width)))
    (list A B C D)))

(define (make-rect-al edge width)
  ; Given one edge and the width between the given edge and
  ; the one parallel, this procedure returns a rectangle as
  ; a list of four edges, where the given edge is the first
  ; in the list.
  (let ((P (start-segment edge))
	(Q (end-segment edge))
	(R (third-vertex edge width))
	(S (third-vertex (flip-line edge) width)))
    (let ((a edge)
	  (b (make-line Q R))
	  (c (make-line R S))
	  (d (make-line S P)))
      (list a b c d))))

; The following two procedures each test (unsafely) the identity
; of their respective types of rectangle objects.

(define (rect-av? rect)
  ; returns true if rect is a list of vertexes
  (number? (car (car rect))))

(define (rect-al? rect)
  ; returns true if rect is a list of pairs
  (pair? (car (car rect))))

; Given the above definitions of a rectangle, the below accessors
; all assume that the rectangle is modeled similarly to the
; following:
;
;	1		 2
;	 +--------------+
;	 |		|
;	 |		|
;	 +--------------+
;	4		 3
;
; where the line AB is the line passed to the original call
; to make-rect and the distance between AB and DC is equal
; to the width argument.

; The vertexes can each be accessed by the same function 
; according to their corresponding number as drawn above:
(define (rect-vertex rect number)
  (define (rec i num)
    (if (= i num)
      rect
      (cdr (rec (+ i 1) num))))
  (define (vertex-av num)
    (car (rec 1 num)))
  (define (vertex-al num)
    (start-segment (car (rec 1 num))))
  (cond ((> number 4) (display "Invalid number; must be [1-4]."))
	((rect-av? rect) (vertex-av number))
	((rect-al? rect) (vertex-al number))
	(else (display "Not a rectangle."))))

; The edges can each be accessed by the same function 
; according to their numbers, where side 1 is the AB segment,
; 2 is BC, 3 is CD, and 4 is DA.
(define (rect-edge rect num)
  (case num
    ((1) (make-line (rect-vertex rect "A") (rect-vertex rect "B")))
    ((2) (make-line (rect-vertex rect "B") (rect-vertex rect "C")))
    ((3) (make-line (rect-vertex rect "C") (rect-vertex rect "D")))
    ((4) (make-line (rect-vertex rect "D") (rect-vertex rect "A")))
    (else (display "Invalid num; must be [1-4]."))))

;;	In terms of your constructors and selectors, create 
;;	procedures that compute the perimeter and the area of
;;	a given rectangle.

(define (rect-perimeter rect)
  (define (iter sum i)
    (if (< i 5)
      (iter (+ sum (length-line (rect-edge rect i))) (+ i 1))
      sum))
  (iter 0 1))

(define (rect-area rect)
  (* (length-line (rect-edge rect 1))
     (length-line (rect-edge rect 2))))
