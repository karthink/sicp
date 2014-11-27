;;-------------
;;EXERCISE 3.5:
;;-------------

;;; Monte Carlo integration is a method of estimating definite
;;; integrals by means of Monte Carlo simula- tion. Consider computing
;;; the area of a region of space de- scribed by a predicate P (x , y)
;;; that is true for points (x , y) in the region and false for points
;;; not in the region. For example, the region contained within a
;;; circle of radius 3 centered at (5, 7) is described by the
;;; predicate that tests whether (x - 5)2 + (y - 7)2 <= 32 . To
;;; estimate the area of the region described by such a predicate,
;;; begin by choosing a rectangle that contains the region. For
;;; example, a rectangle with diagonally opposite corners at (2, 4)
;;; and (8, 10) con- tains the circle above. The desired integral is
;;; the area of that portion of the rectangle that lies in the region.
;;; We can estimate the integral by picking, at random, points (x , y)
;;; that lie in the rectangle, and testing P (x , y) for each point to
;;; determine whether the point lies in the region. If we try this
;;; with many points, then the fraction of points that fall in the
;;; region should give an estimate of the proportion of the rectangle
;;; that lies in the region. Hence, multiplying this fraction by the
;;; area of the entire rectangle should produce an estimate of the
;;; integral.
