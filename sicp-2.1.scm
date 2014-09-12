;;------------
;;EXERCISE 2.1
;;------------

;;; Define a better version of make-rat that handles both positive and
;;; negative arguments. Make-rat should normalize the sign so that if
;;; the rational number is positive, both the numerator and
;;; denominator are positive, and if the rational number is negative,
;;; only the numerator is negative.

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

(setq last-kbd-macro
   [?\C-y ?\C-x ?\C-x ?\M-f ?\C-f ?\C-x ?r ?n ?a ?\C-f ?\C-x ?r ?n ?b ?\C-a ?\M-k ?\; ?\; ?\; ?\M-q ?\M-o ?\M-o ?\; ?- tab ?e ?x ?e ?r ?c ?i ?s ?e ?  ?\C-x ?r ?i ?a ?\C-f ?. ?\C-x ?r ?i ?b tab ?\C-k ?\M-\]])
