;;----------
;;PRIMITIVES
;;----------
(define (fast-expt b n)
  (define (even? n)
    (= (remainder n 2) 0 ))
  (define (fast-expt-iter b n a)
    (cond ((= n 0) a)
          ((even? n) (fast-expt-iter (square b) (/ n 2) a))
          (else (fast-expt-iter b (- n 1) (* a b)))))
  (fast-expt-iter b n 1))

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

;;------------
;;EXERCISE 2.4
;;------------

;;; Here is an alternative procedural representation of pairs. For
;;; this representation, verify that (car (cons x y)) yields x for any
;;; objects x and y. 

(define (my-cons x y)
  (lambda (m) (m x y)))

(define (my-car z)
  (z (lambda (p q) p)))

;;; Hint: To verify that this works, make use of the substitution
;;; model of section 1-1-5.

;;; Verify first:
(my-car (my-cons 0 1))                  ;0

;;; my-cons returns a function that accepts a function and applies it
;;; to x and y

;;; my-car (when-called) applies my-cons to a function that takes two
;;; arguments and returns the first one.

;;; Substitution model:
(my-car (my-cons 0 1))
(my-car (lambda (m) (m x y)))
((lambda (m) (m x y)) (lambda (p q) p))
((lambda (p q) p) x y)
x

;;;  What is the corresponding definition of cdr?
(define  (my-cdr z)
  (z (lambda (p q) q)))

(my-cdr (my-cons 0 1))                  ;1

;;------------
;;EXERCISE 2.5
;;------------

;;; Show that we can represent pairs of nonnegative integers using
;;; only numbers and arithmetic operations if we represent the pair a
;;; and b as the integer that is the product 2^a3^b. Give the
;;; corresponding definitions of the procedures cons, car, and cdr.

;;; Definitions of cons, car and cdr

(define (econs a b)
  (* (fast-expt 2 a) (fast-expt 3 b)))

(define (divides-how-many-times z k times)
  (if (not (= (remainder z k) 0))
      times
      (divides-how-many-times (/ z k) k (+ times 1))))

(define (ecar z)
  (divides-how-many-times z 2 0))

(define (ecdr z)
  (divides-how-many-times z 3 0))

(ecar (econs 5 4))                      ;5
(ecdr (econs 5 4))                      ;4

;;; We need to show that the mapping a,b -> 2^a3^b is one-one.

;;; Suppose there exist two pairs of nonnegative integers a,b and p,q
;;; for which 2^a3^b = 2^p3^q. 2^(a-p) 3^(b-q) = 1. Since 2 and 3 are
;;; (relatively) prime, the LHS represents a prime factorization of 1,
;;; which is 1 itself. So 2^(a-p) = 3^(b-q) = 1, or a,b = p,q.

;;------------
;;EXERCISE 2.6
;;------------

;;; In case representing pairs as procedures wasn't mind-boggling
;;; enough, consider that, in a language that can manipulate
;;; procedures, we can get by without numbers (at least insofar as
;;; nonnegative integers are concerned) by implementing 0 and the
;;; operation of adding 1 as 

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f)
    (lambda (x) (f ((n f) x)))))

;;; This representation is known as Church numerals , after its
;;; inventor, Alonzo Church, the logician who invented the lambda calculus.

;;; Define one and two directly (not in terms of zero and add-1).
;;; (Hint: Use substitution to evaluate (add-1 zero)). Give a direct
;;; definition of the addition procedure + (not in terms of repeated
;;; application of add-1).

(define one
  (lambda (f)
    (lambda (x) (f x))))                     ;basically f

(define two
  (lambda (f)
    (lambda (x) (f (f x)))))                 ;f(f())

(define (church+ n1 n2)
  (lambda (f)
    (lambda (x) ((n2 f) ((n1 f) x)))))

;;; (n1 f) is a lambda that composes f with itself n1 times. ((n1 f) x) is
;;; (f (f (f ....f x). To apply f (n1 + n2) times on x, we call (n2 f) on
;;; the result of ((n1 f) x).

;;; Also, we can write: (define (add n1 n2) ((n1 add-1) n2))

;;------------
;;EXERCISE 2.7
;;------------

;;; Alyssa's program is incomplete because she has not specified the
;;; implementation of the interval abstraction. Here is a definition
;;; of the interval constructor:

(define (make-interval a b) (cons a b))

;;; Define selectors upper-bound and lower-bound to complete the
;;; implementation.

(define (upper-bound interval)
  (max (car interval) (cdr interval)))

(define (lower-bound interval)
  (min (car interval) (cdr interval)))


(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;;------------
;;EXERCISE 2.8
;;------------

;;; Using reasoning analogous to Alyssa's, describe how the difference
;;; of two intervals may be computed. Define a corresponding
;;; subtraction procedure, called sub-interval.

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;;;  OR

(define (sub-interval x y)
  (add-interval x (make-interval (- (upper-bound y))
                                 (- (lower-bound y)))))


;;------------
;;EXERCISE 2.9
;;------------

;;; The width of an interval is half of the difference between its
;;; upper and lower bounds. The width is a measure of the uncertainty
;;; of the number specified by the interval. For some arithmetic
;;; operations the width of the result of combining two intervals is a
;;; function only of the widths of the argument intervals, whereas for
;;; others the width of the combination is not a function of the
;;; widths of the argument intervals. Show that the width of the sum
;;; (or difference) of two intervals is a function only of the widths
;;; of the intervals being added (or subtracted). Give examples to
;;; show that this is not true for multiplication or division.

;;; Sum: let the widths be w1 and w2, and the lower and upper bounds
;;; be l1,l2,u1,u2. Then the sum is (make-interval (+ l1 l2) (+ u1
;;; u2)), which has width

;; (- (+ u1 u2) (+ l1 l2)) 

;;; Which is (u2 - l2) + (u1 - l1) = 2(w2 + w1). The same applies to
;;; subtraction.

;;; Mult/div:

(define (width-interval x) (/ (- (upper-bound x) (lower-bound x)) 2))

(define i1 (make-interval 0 2))
(define i2 (make-interval 4 1))

(width-interval i1)                     ; 1
(width-interval i2)                     ; 3:2
(width-interval (mul-interval i1 i2))   ; 4

(define j1 (make-interval 1 3))         ; same width as i1
(define j2 (make-interval 1 4))         ; same width as i2

(width-interval j1)                     ; 1
(width-interval j2)                     ; 3:2
(width-interval (mul-interval j1 j2))   ; 11/2 != 4

;;; The same holds for division because it is defined in terms of
;;; mul-interval

;;-------------
;;EXERCISE 2.10
;;-------------

;;; Ben Bitdiddle, an expert systems programmer, looks over Alyssa's
;;; shoulder and comments that it is not clear what it means to divide
;;; by an interval that spans (includes) zero. Modify Alyssa's code to
;;; check for this condition and to signal an error if it occurs.

(define (div-interval x y)
  (let ((ly (lower-bound y)) (uy (upper-bound y)))
    (cond ((same-sign? ly uy)
           (mul-interval x
                         (make-interval (/ 1.0 (upper-bound y))
                                        (/ 1.0 (lower-bound y)))))
          (else (error "Interval includes 0")))))

;;-------------
;;EXERCISE 2.11
;;-------------

;;; In passing, Ben also cryptically comments: "By testing the signs
;;; of the endpoints of the intervals, it is possible to break
;;; mul-interval into nine cases, only one of which requires more than
;;; two multiplications." Rewrite this procedure using Ben's
;;; suggestion.

;; 9 cases:

;; | x to the right of 0 |         | y to the right of 0 |
;; | x to the left of 0  |    X    | y to the left of 0  |
;; | x containing 0      |         | y containing 0      |

;; 1. Both to the right of zero:
;; (* (lower-bound x) (lower-bound y))
;; (* (upper-bound x) (upper-bound y))

;; 2. Both to the left of zero:
;; (* (upper-bound x) (upper-bound y))
;; (* (lower-bound x) (lower-bound y))

;; 3. x to the left, y to the right
;; (* (lower-bound x) (upper-bound y))
;; (* (upper-bound x) (lower-bound y))

;; 4. x to the right, y to the left
;; (the opposite of 3)

;; 5. x containing 0, y to the right
;; (* (lower-bound x) (upper-bound y))
;; (* (upper-bound x) (upper-bound y))

;; 6. x containing 0, y to the left
;; (* (upper-bound x) (lower-bound y))
;; (* (lower-bound x) (lower-bound y))

;; 7,8. y containing 0, x to the left/right
;; (opposites of 5 & 6)

;; 9. x and y both contain 0
;; (min (* (lower-bound x) (upper-bound y))
;;      (* (upper-bound x) (lower-bound y)))
;; (max (* (lower-bound x) (lower-bound y))
;;      (* (upper-bound x) (upper-bound y)))

;;; Here's the function

(define (mul-interval x y)
  (let ((lx (lower-bound x))
        (ly (lower-bound y))
        (ux (upper-bound x))
        (uy (upper-bound y)))
    (cond ((and  (same-sign? lx ux)
                 (same-sign? lx uy)
                 (same-sign? lx ly))
           ;; Cases 1 & 2
           (make-interval (* lx ly) (* ux uy)))

          ((and (same-sign? lx ux)
                (same-sign? ly uy))
           ;; Cases 3 & 4
           (make-interval (* lx uy) (* ux ly)))

          ((and (not (same-sign? lx ux))
                (same-sign? ly uy))
           ;; Cases 5 & 6
           (if (> ly 0) 
               (make-interval (* lx uy) (* ux uy))
               (make-interval (* lx ly) (* ux ly))))

          ((and (not (same-sign? ly uy))
                (same-sign? lx ux))
           ;; Cases 7 & 8
           (if (> lx 0) 
               (make-interval (* ly ux) (* uy ux))
               (make-interval (* ly lx) (* uy lx))))

          (else
           (make-interval (min (* lx uy)
                               (* ux ly))
                          (max (* lx ly)
                               (* ux uy)))))))

;;; It might be possible to make this much cleaner with a more
;;; suitable helper function than same-sign?

;;; Here's how to test it.
(define (test-mul x1 x2 y1 y2)
  (let ((i1 (make-interval x1 x2))
        (i2 (make-interval y1 y2)))
    (display i1)
    (newline)
    (display i2)
    (newline)
    (mul-interval i1 i2)))

  
;;; After debugging her program, Alyssa shows it to a potential user,
;;; who complains that her program solves the wrong problem. He wants
;;; a program that can deal with numbers represented as a center value
;;; and an additive tolerance; for example, he wants to work with
;;; intervals such as 3.5 +/- 0.15 rather than [3.35, 3.65]. Alyssa
;;; returns to her desk and fixes this problem by supplying an
;;; alternate constructor and alternate selectors:

;;-------------
;;EXERCISE 2.12
;;-------------

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;;; Define a constructor make-center-percent that takes a center and a
;;; percentage tolerance and produces the desired interval. You must
;;; also define a selector percent that produces the percentage
;;; tolerance for a given interval. The center selector is the same as
;;; the one shown above.

(define (make-center-percent center percent)
  (make-center-width center (/ (* percent center) 100)))

(define (percent interval)
  (* 100 (/ (width interval) (center interval))))

;;-------------
;;EXERCISE 2.13
;;-------------

;;; Show that under the assumption of small percentage tolerances
;;; there is a simple formula for the approximate percentage tolerance
;;; of the product of two intervals in terms of the tolerances of the
;;; factors. You may simplify the problem by assuming that all numbers
;;; are positive.

;; In the (center,percent) formulation:
;; (C1,p1): [C1 - p1 C1, C1 + p1 C1]
;; (C2,p2): [C2 - p2 C2, C2 + p2 C2]

;; All positive, so the product is:

;; [C1 C2 - (p1 + p2)C1 C2 + p1 p2 C1 C2,
;;  C1 C2 + (p1 + p2) C1 C2 + p1 p2 C1 C2]

;; For small p1 & p2, we can neglect the second order term.

;; [C1 C2 - (p1 + p2) C1 C2, C1 C2 + (p1 + p2) C1 C2]
;; (C1 C2, p1 + p2)

;; So the percentages add.

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
 
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;;-------------
;;EXERCISE 2.14
;;-------------

;;; Demonstrate that Lem is right. Investigate the behavior of the
;;; system on a variety of arithmetic expressions. Make some intervals
;;; A and B, and use them in computing the expressions A/A and A/B.
;;; You will get the most insight by using intervals whose width is a
;;; small percentage of the center value. Examine the results of the
;;; computation in center-percent form (see Exercise 2-12).

;; (setq last-kbd-macro
;;    [?\M-d ?\C-x ?r ?n ?a ?\C-x ?r ?n ?b ?\C-f ?\C-x ?r ?n ?b ?\C-  ?\C-a ?\C-w ?\C-d ?\C-d ?\C-b ?\C-d ?\M-o ?\M-o ?\; ?- tab ?e ?x ?e ?r ?c ?i ?s ?e ?  ?\C-x ?r ?i ?a ?\C-f ?. ?\C-x ?r ?i ?b tab ?\C-k ?\C-n ?\; ?\; ?\; ?  ?\M-q ?\M-\]])

(define A (make-center-width 1000 5))
(define B (make-center-width 300 1))

(div-interval A A)                      ;(.99004 . 1.01005)
;;; This should be (1.0 .. 1.0), because A/A = 1.0 irrespective of our
;;; uncertainty about A. This is one source of the error.

;;--------------------
;;EXERCISE 2.15 & 2.16
;;--------------------

;;; Eva Lu Ator, another user, has also noticed the different
;;; intervals computed by different but algebraically equivalent
;;; expressions. She says that a formula to compute with intervals
;;; using Alyssa's system will produce tighter error bounds if it can
;;; be written in such a form that no variable that represents an
;;; uncertain number is repeated. Thus, she says, par2 is a "better"
;;; program for parallel resistances than par1. Is she right? Why?

;;; Explain, in general, why equivalent algebraic expressions may lead
;;; to different answers. Can you devise an interval-arithmetic
;;; package that does not have this shortcoming, or is this task
;;; impossible? (Warning: This problem is very difficult.)

;;; She's right, because every additional appearance of an uncertain
;;; variable widens the uncertainty interval--even repeated
;;; appearances of the same variable. This is shown by A/A in ex. 2.14
;;; above. 

;;; Designing an interval-arithmetic package: Doesn't seem impossible,
;;; but I will return to this when I can use lists and other data
;;; structures.

