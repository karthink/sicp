;;; Solutions to problems 3.9 through 3.11 are on paper.
;;;
;;; Some possible points of note are:

;;; 1. When a procedure is defined, its body is _not_ evaluated
;;; irrespective of what it contains. The code is stored somewhere,
;;; and a pointer is created to the defining environment/frame.

;;; 2. In the diagrams 3.9-11 asks you to draw, the output of any
;;; given procedure is not shown. It only matters if it's a procedure
;;; that is then called, like in ((acc 'deposit) 40).

;;; 3. The frame corresponding to a procedure call that has terminated
;;; and is not pointed to by any other frame is of no relevance.

;;; 4. A let expression is just a lambda that creates an environment when
;;; it is evaluated.


