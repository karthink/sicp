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


