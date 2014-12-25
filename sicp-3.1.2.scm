(define (estimate-pi trials)
(sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
(= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))


;;-------------
;;EXERCISE 3.5:
;;-------------

;;; Monte Carlo integration is a method of estimating definite
;;; integrals by means of Monte Carlo simula- tion. Consider computing
;;; the area of a region of space de- scribed by a predicate P (x , y)
;;; that is true for points (x , y) in the region and false for points
;;; not in the region. For example, the region contained within a
;;; circle of radius 3 centered at (5, 7) is described by the
;;; predicate that tests whether (x - 5)^2 + (y - 7)^2 <= 32 . To
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

;;; Implement Monte Carlo integration as a procedure estimate-
;;; integral that takes as arguments a predicate P, upper and lower
;;; bounds x1, x2, y1, and y2 for the rectangle, and the number of
;;; trials to perform in order to produce the estimate. Your
;;; procedure should use the same monte-carlo procedure that was used
;;; above to estimate pi. Use your estimate- integral to produce an
;;; estimate of pi by measuring the area of a unit circle.

;;; You will find it useful to have a procedure that returns a number
;;; chosen at random from a given range. The follow- ing
;;; random-in-range procedure implements this in terms of the random
;;; procedure used in Section 1.2.6, which returns a nonnegative
;;; number less than its input.

(define (random-in-range low high)
  (let ((range (- high low 0.0)))
    (+ low (random range))))

(define (estimate-integral P x1 y1 x2 y2 trials)
  (define (inside?) (P (random-in-range x1 x2)
                       (random-in-range y1 y2)))
  
  (* (monte-carlo trials inside?)
     (- x2 x1)
     (- y2 y1)))

;;; Unit circle:
(define (unit-circle x y)
  (or (<= (+ (* x x) (* y y))
          1)))

;;; Exercise 3.6: It is useful to be able to reset a random-number
;;; generator to produce a sequence starting from a given value.
;;; Design a new rand procedure that is called with an ar- gument that
;;; is either the symbol generate or the symbol reset and behaves as
;;; follows: (rand 'generate) produces a new random number ((rand
;;; 'reset) <new-value>) resets the internal state variable to the
;;; designated <new-value>. Thus, by resetting the state, one can
;;; generate repeatable se- quences. These are very handy to have when
;;; testing and debugging programs that use random numbers.

;;; I can't test this solution because rand/rand-update are not
;;; defined in mit-scheme (or Gambit):

(define rand
  (let ((x random-init))
    (lambda (arg)
      (cond ((eq? arg 'generate)
             (set! x (rand-update x))
             x)
            ((eq? arg 'reset)
             (lambda (seed)
               (set! x seed)))))))




