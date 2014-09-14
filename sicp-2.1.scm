;;----------
;;PRIMITIVES
;;----------
(define (square n) (* n n))
(define (average a b) (/ (+ a b) 2))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define  (same-sign? a b)
  (or (and (> a 0) (> b 0))
      (and (< a 0) (< b 0))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat r) 
   (newline) 
   (display (numer r)) 
   (display "/") 
   (display (denom r))) 

;;------------
;;EXERCISE 2.1
;;------------

;;; Define a better version of make-rat that handles both positive and
;;; negative arguments. Make-rat should normalize the sign so that if
;;; the rational number is positive, both the numerator and
;;; denominator are positive, and if the rational number is negative,
;;; only the numerator is negative.

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons ((if (same-sign? n d) + -) (abs (/ n g)))
          (abs (/ d g)))))

;;------------
;;EXERCISE 2.2
;;------------

;;; Consider the problem of representing line segments in a plane.
;;; Each segment is represented as a pair of points: a starting point
;;; and an ending point. Define a constructor make-segment and
;;; selectors start-segment and end-segment that define the
;;; representation of segments in terms of points. Furthermore, a
;;; point can be represented as a pair of numbers: the x coordinate
;;; and the y coordinate. Accordingly, specify a constructor
;;; make-point and selectors x-point and y-point that define this
;;; representation. Finally, using your selectors and constructors,
;;; define a procedure midpoint-segment that takes a line segment as
;;; argument and returns its midpoint (the point whose coordinates are
;;; the average of the coordinates of the endpoints). To try your
;;; procedures, you'll need a way to print points:

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (make-segment beg end) (cons beg end)) 
(define (start-segment s) (car s)) 
(define (end-segment s) (cdr s))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (midpoint-segment s)
  (make-point (average
               (x-point (start-segment s))
               (x-point (end-segment s)))
              (average
               (y-point (start-segment s))
               (y-point (end-segment s)))))



;; (setq last-kbd-macro
;;    [?\C-y ?\C-x ?\C-x ?\M-f ?\C-f ?\C-x ?r ?n ?a ?\C-f ?\C-x ?r ?n ?b ?\C-a ?\M-k ?\; ?\; ?\; ?\M-q ?\M-o ?\M-o ?\; ?- tab ?e ?x ?e ?r ?c ?i ?s ?e ?  ?\C-x ?r ?i ?a ?\C-f ?. ?\C-x ?r ?i ?b tab ?\C-k ?\M-\]])

;;------------
;;EXERCISE 2.3
;;------------

;;; Implement a representation for rectangles in a plane. (Hint: You
;;; may want to make use of Exercise 2-2.) In terms of your
;;; constructors and selectors, create procedures that compute the
;;; perimeter and the area of a given rectangle. Now implement a
;;; different representation for rectangles. Can you design your
;;; system with suitable abstraction barriers, so that the same
;;; perimeter and area procedures will work using either
;;; representation?

;;; We can represent make-rec with two line segments at right angles
;;; that share one point. It is up to the user to specify segments
;;; that meet at one end in right angles

;;; These are the primitives, at the lower level of abstraction.
(define (make-rec s1 s2) (cons s1 s2))

(define (first-side r) (car r))
(define (secnd-side r) (cdr r))

;;; These are the rectangle functions
(define (length s) (sqrt (+ (square (- (x-point (start-segment s))
                                       (x-point (end-segment s))))
                            (square (- (y-point (start-segment s))
                                       (y-point (end-segment s)))))))

(define (area-rec r) (* (length (first-side r))
                        (length (secnd-side r))))

(define (peri-rec r) (* 2 (+ (length (first-side r))
                             (length (secnd-side r)))))

;;; Here's a second way of defining rectangles: In terms of three
;;; points, the second one of which is a common vertex.

(define (make-rec p1 p2 p3) (cons (cons p1 p3) p2))

(define (first-side r) (make-segment (car (car r)) (cdr r)))
(define (secnd-side r) (make-segment (cdr (car r)) (cdr r)))




