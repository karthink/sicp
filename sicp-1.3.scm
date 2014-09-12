;;; Required primitives

(define (inc x) (+ x 1))
(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))
(define tolerance 0.00001)
(define (average-damp f)
  (lambda (y) (/ (+ y (f y)) 2)))

;;--------------
;; EXERCISE 1.29
;;--------------

;; Simpson's Rule is a more accurate method of numerical integration
;; than the method illustrated above. Using Simpson's Rule, the
;; integral of a function f between a and b is approximated as

;; (h/3)[y0+4y1+2y2+4y3+2y4+...+2yn-2+4yn-1+yn]

;; where h=(b-a)/n, for some even integer n, and yk=f(a+kh).
;; (Increasing n increases the accuracy of the approximation.) Define
;; a procedure that takes as arguments f, a, b, and n and returns the
;; value of the integral, computed using Simpson's Rule. Use your
;; procedure to integrate cube between 0 and 1 (with n = 100 and n =
;; 1000), and compare the results to those of the integral procedure
;; shown above.

;; (define (simpson f a b n)
;;   'your-code-here)

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;;; Rewriting the sum as:
;;; (h/3)[2(y0+2y1)+2(y2+2y3)+...+2(yn-2+2yn-1)] + (h/3)[yn - y0]
;;; and summing over the pairs (y_k + y_k+1):

(define (simpson f a b n)
  (define h (/ (- b a) n))
  
  (define (y k)
    (f (+ a (* k h))))
  
  (define (ypair k)
    (+ (* 2 (y k))
       (* 4 (y (+ k 1)))))
  
  (define (add-2 k)
    (+ k 2))
  
  (* (/ h 3) (+ (sum ypair 0 add-2 (- n 1))
                (- (f b) (f a)))))

;;; Test

(define (cube x) (* x x x))
(simpson cube 0 1.0 10)                 ;0.25

(define (inc x) (+ x 1))
(simpson inc 0 1.0 10)                  ;1.5

(define (identity x) x)
(simpson identity 0 1.0 10)             ;0.5

(define (const x) 1)
(simpson const 0 1.0 10)                ;1.0

(define (inv x) (/ 1 x))
(simpson inv 1.0 2.7182 10)             ;0.999999


;;; Simpson's rule converges much faster than the "integral" function.

;;-------------
;;EXERCISE 1.30
;;-------------

;;; The sum procedure above generates a linear recursion. The
;;; procedure can be rewritten so that the sum is performed
;;; iteratively. Show how to do this by filling in the missing
;;; expressions in the following definition:

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))


;;--------------
;; EXERCISE 1.31
;;--------------

;;; The sum procedure is only the simplest of a vast number of similar
;;; abstractions that can be captured as higher-order procedures.
;;; Write an analogous procedure called product that returns the
;;; product of the values of a function at points over a given range.
;;; Try both a recursive and an iterative approach. Show how to define
;;; factorial in terms of product. Also use product to compute
;;; approximations to using the formula

;;; pi/4 = (2/3) (4/3) (4/5) (6/5) (6/7) (6/7)...

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (const x) 1)
(define (identity x) x)
(define (inv x) (/ 1 x))
(define (inc x) (+ x 1))

;;; factorial
(define (fac n) (product identity 1 inc n))

;;; pi/4 = (2/3) (4/3) (4/5) (6/5) (6/7) (6/7)...
(define (pi4 n)
  (define (term k)
    (if (even? k)
        (/ (+ k 2) (+ k 1))
        (/ (+ k 1) (+ k 2))))
  (define (next k)
    (+ k 1))
  (product term 1.0 next n))

;;; Iterative product
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

;;-------------
;;EXERCISE 1.32
;;-------------

;;; Show that sum and product (exercise 1.31) are both special cases
;;; of a still more general notion called accumulate that combines a
;;; collection of terms, using some general accumulation function:

(accumulate combiner null-value term a next b)

;;; Accumulate takes as arguments the same term and range
;;; specifications as sum and product, together with a combiner
;;; procedure (of two arguments) that specifies how the current term
;;; is to be combined with the accumulation of the preceding terms and
;;; a null-value that specifies what base value to use when the terms
;;; run out. Write accumulate and show how sum and product can both be
;;; defined as simple calls to accumulate. Write two procedures, one
;;; that generates a recursive process and one iterative.

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value
                                     term (next a) next b))))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

;;-------------
;;EXERCISE 1.33
;;-------------

;;; You can obtain an even more general version of accumulate
;;; (exercise 1.32) by introducing the notion of a filter on the terms
;;; to be combined. That is, combine only those terms derived from
;;; values in the range that satisfy a specified condition. The
;;; resulting filtered-accumulate abstraction takes the same arguments
;;; as accumulate, together with an additional predicate of one
;;; argument that specifies the filter. Write filtered-accumulate as a
;;; procedure.

;;; Recursive
(define (filtered-accumulate combiner null-value term a next b filter?)
  (if (> a b)
      null-value
      (if (filter? a)
          (combiner (if (filter? a)
                        (term a)
                        null-value)
                    (filtered-accumulate combiner
                                         null-value
                                         term
                                         (next a)
                                         next
                                         b
                                         filter?)))))

;;; Iterative
(define (filtered-accumulate combiner null-value term a next b filter?)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (if (filter? a)
                           (combiner result (term a))
                           result))))
  (iter a null-value))

;;; Sum of squares of prime numbers
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(filtered-accumulate + 0 square 2 inc 10 prime?)

;;-------------
;;EXERCISE 1.34
;;-------------

;;; Show that the golden ratio (section 1.2.2) is a fixed point of the
;;; transformation x -> 1+1/x, and use this fact to compute phi by means
;;; of the fixed-point procedure.

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;;; Needed later
(define (average-damp f)
  (lambda (y) (/ (+ y (f y)) 2)))

(define phi
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)) ; 1.6180327868852458


;;; Modify fixed-point so that it prints the sequence of
;;; approximations it generates, using the newline and display
;;; primitives shown in exercise 1.22. Then find a solution to xx=1000
;;; by finding a fixed point of x->log(1000)log(x). (Use Scheme's
;;; primitive log procedure, which computes natural logarithms.)
;;; Compare the number of steps this takes with and without average
;;; damping. (Note that you cannot start fixed-point with a guess of
;;; 1, as this would cause division by log(1)=0.)

(define (fixed-point-verbose f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (cond ((close-enough? guess next) next)
            (else (newline)
                  (display next) 
                  (try next)))))
  (try first-guess))

;; (fixed-point-verbose (lambda (x) (+ 1 (/ 1 x))) 1.0)
(fixed-point-verbose (lambda (x) (/ (log 1000) (log x))) 4.0) ; 4.555539183677709

;;-------------
;;EXERCISE 1.37
;;-------------

;;; An infinite continued fraction is an expression of the form

;;; f = N1 / (D1 + N2/(D2 + N3/(D3 + ...

;;; As an example, one can show that the infinite continued fraction
;;; expansion with the Ni and the Di all equal to 1 produces 1/phi, where
;;; phi is the golden ratio (described in section 1.2.2). One way to
;;; approximate an infinite continued fraction is to truncate the
;;; expansion after a given number of terms. Such a truncation-a
;;; so-called k-term finite continued fraction-has the form

;;; f = N1 / (D1 + N2 / (D2 + ... + Nk / Dk

;;; Suppose that n and d are procedures of one argument (the term
;;; index i that return the Ni and Di of the terms of the continued
;;; fraction. Define a procedure cont-frac such that evaluating
;;; (cont-frac n d k) computes the value of the k-term finite
;;; continued fraction.


(define (cont-frac n d k)
  (define (cont-frac-rec i)
    (if (< k i)
        0
        (/ (n i) (+ (d i)
                    (cont-frac-rec (+ i 1))))))
  (cont-frac-rec 1))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)                          ; .6180555555555556

;;; k should be >= 11

;;; The above definition was recursive. Here's an iterative one. The
;;; change is to count down from k instead of counting up from 1,
;;; while accumulating results in frac.

(define (cont-frac n d k)
  (define (cont-frac-iter k frac)
    (if (= k 0)
        frac
        (cont-frac-iter (- k 1)
                        (/ (n k) (+ (d k) frac)))))
  (cont-frac-iter k 0))

;;-------------
;;EXERCISE 1.38
;;-------------

;;; e - 2 = (cont-frac (all ni = 1) (di = 1, 2, 1, 1, 4, 1, 1, 6, 1, 1...) inf)

(define (n-euler i) 1.0)
(define (d-euler i) (if (= 0 (remainder (+ 1 i) 3))
                        (* 2 (/ (+ 1 i) 3))
                        1))

(+ 2 (cont-frac n-euler d-euler 10))    ; 2.7182817182817183


;;-------------
;;EXERCISE 1.39
;;-------------

;;; tan x = x / (1 - x^2 / (3 - x^2 / ( 5 - x^2 / ...

(define (tan-cf x k)
  (define (n-tan i) (if (= i 1) x (- (* x x))))
  (define (d-tan i) (- (* 2 i) 1))
  (cont-frac n-tan d-tan k))

(tan-cf (/ 3.1415926 4) 18)             ; .9999999732051038


;;-------------
;;EXERCISE 1.40
;;-------------

;;;  Exercise 1.40. Define a procedure cubic that can be used together
;;;  with the newtons-method procedure in expressions of the form

;; (define (cubic a b c)
;;   'your-answer-here)
;; (newtons-method (cubic a b c) 1)

;;; to approximate zeros of the cubic x3+ax2+bx+c.

;;; Newton's method definitons:

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;;; cubic

(define (cubic a b c)
  (lambda (x) (+ c (* b x) (* a x x) (* x x x))))

;;; Test
(newtons-method (cubic -1 0 0) 1.5)     ;1.000000000000071
(newtons-method (cubic -1 0 0) 0.5)     ;-4.0295529188748996e-10 ~ 0

;;-------------
;;EXERCISE 1.41
;;-------------

;;; Define a procedure double that takes a procedure of one argument
;;; as argument and returns a procedure that applies the original
;;; procedure twice. For example, if inc is a procedure that adds 1 to
;;; its argument, then (double inc) should be a procedure that adds 2.

(define (double f)
  (lambda (y) (f (f y))))

(define (inc x) (+ x 1))
(define (square x) (* x x))

(((double (double double)) inc) 5)      ;21

;;; (double double) := (lambda (f) (double (double f)))
;;; (double (double double)) := (lambda (f) (double (double (double (double f)))))
;;; When applied to inc:
;;; (((lambda (f) (double (double (double (double f))))) inc) x)
;;; ((double (double (double (double inc)))) x)
;;; ((double (double (double (lambda (y) (inc (inc y)))))) x)
;;; ((double (double (lambda (y) (inc (inc (inc (inc y))))))) x)
;;; ((double (lambda (y) (inc (inc (inc (inc (inc (inc (inc (inc y)))))))))) x)
;;; ((lambda (y) (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc x))))))))))))))))) x)
;;; So inc is applied 16 times, raising x by 1 each time.

;;-------------
;;EXERCISE 1.42
;;-------------

;;; Let f and g be two one-argument functions. The composition f after
;;; g is defined to be the function x f(g(x)). Define a procedure
;;; compose that implements composition.

(define (compose f g)
  (lambda (y) (f (g y))))

((compose square inc) 6)                ; 49

;;-------------
;;EXERCISE 1.43
;;-------------

;;; If f is a numerical function and n is a positive integer, then we
;;; can form the nth repeated application of f, which is defined to be
;;; the function whose value at x is f(f(...(f(x))...)). For example, if f
;;; is the function x->x+1, then the nth repeated application of f is
;;; the function x->x+n. If f is the operation of squaring a number,
;;; then the nth repeated application of f is the function that raises
;;; its argument to the 2nth power. Write a procedure that takes as
;;; inputs a procedure that computes f and a positive integer n and
;;; returns the procedure that computes the nth repeated application
;;; of f.

;;; Recursive
(define (repeated f n)
  (if (= n 1) f
      (compose f (repeated f (- n 1)))))

;;; Iterative
(define (repeated f n)
  
  (define (repeated-iter f k f-rep)
    (if (= k n)
        f-rep
        (repeated-iter f (+ k 1) (compose f f-rep))))
  
  (repeated-iter f 1 f))

;;; Test it
((repeated square 2) 5)                         ;625
((repeated sqrt 2) 625)                         ;5.0

;;; Using 'repeated to run 'fixed-point 8 times and find the sqrt of
;;; 25

((repeated (lambda (x)
             (/ (+ x (/ 25 x))
                2))
           8) 1.0)                      ;5.0

;;-------------
;;EXERCISE 1.44
;;-------------

;;; The idea of smoothing a function is an important concept in signal
;;; processing. If f is a function and dx is some small number, then
;;; the smoothed version of f is the function whose value at a point x
;;; is the average of f(x-dx), f(x), and f(x+dx). Write a procedure
;;; smooth that takes as input a procedure that computes f and returns
;;; a procedure that computes the smoothed f. It is sometimes valuable
;;; to repeatedly smooth a function (that is, smooth the smoothed
;;; function, and so on) to obtained the n-fold smoothed function.
;;; Show how to generate the n-fold smoothed function of any given
;;; function using smooth and repeated from exercise 1.43.

(define dx 0.1)

(define (smooth f)
  (lambda (x) (+ (f x) (f (- x dx)) (f (+ x dx)))))


;;-------------
;;EXERCISE 1.45
;;-------------

;;; We saw in section 1.3.3 that attempting to compute square roots by
;;; naively finding a fixed point of y->x/y does not converge, and
;;; that this can be fixed by average damping. The same method works
;;; for finding cube roots as fixed points of the average-damped
;;; y->x/y^2. Unfortunately, the process does not work for fourth
;;; roots -- a single average damp is not enough to make a fixed-point
;;; search for y->x/y^3 converge. On the other hand, if we average
;;; damp twice (i.e., use the average damp of the average damp of
;;; y->x/y^3) the fixed-point search does converge. Do some
;;; experiments to determine how many average damps are required to
;;; compute nth roots as a fixed-point search based upon repeated
;;; average damping of y->x/y^n-1. Use this to implement a simple
;;; procedure for computing nth roots using fixed-point, average-damp,
;;; and the repeated procedure of exercise 1.43. Assume that any
;;; arithmetic operations you need are available as primitives.

;;; Some primitives we need:
(define (square y) (* y y))

(define (exp y n)
  (cond ((= n 0) 1)
        ((even? n) (square (exp y (/ n 2))))
        (else (* y (square (exp y (/ (- n 1) 2)))))))

;;; A general nth-root function: nth root of x with k-time
;;; average-damping.
(define (nth-root-unstable x n k)
  (fixed-point
   ((repeated average-damp k)
    (lambda (y) (/ x (exp y (- n 1)))))
   1.0))

;;; Required k for a given n   
 ;; |  n | k |
 ;; |  2 | 1 |
 ;; |  3 | 1 |
 ;; |  4 | 2 |
 ;; |  5 | 2 |
 ;; |  6 | 2 |
 ;; |  7 | 2 |
 ;; |  8 | 3 |
 ;; |  9 | 3 |
 ;; | 10 | 3 |
 ;; | 11 | 3 |
 ;; | 12 | 3 |
 ;; | 13 | 3 |
 ;; | 14 | 3 |
 ;; | 15 | 3 |
 ;; | 16 | 4 |

;;; k=2 works for 4 powers, 3-7. k=3 works for 8 powers, 8-15.
;;; Hunch: k will work for 2^k powers. (k=4 works for 16-31 and fail on k=32. This can be confirmed.)

;;; So k will work for n = 2^k to 2^(k+1) - 1. So given an n, we can find k by taking the log of n to base 2 and discarding the non-integer bit:

(define (times-avg-damp n)
  (truncate (/ (log n) (log 2))))

(define (nth-root x n)
  (nth-root-unstable x n (times-avg-damp n)))

;;; Test:
(nth-root 15625 64)                     ;1.1628626935731257
(nth-root 6.4e14 80)                    ;1.5313603602212593

;;; Done!

;;-------------
;;EXERCISE 1.46
;;-------------

;;;  Several of the numerical methods described in this chapter are
;;;  instances of an extremely general computational strategy known as
;;;  iterative improvement. Iterative improvement says that, to
;;;  compute something, we start with an initial guess for the answer,
;;;  test if the guess is good enough, and otherwise improve the guess
;;;  and continue the process using the improved guess as the new
;;;  guess. Write a procedure iterative-improve that takes two
;;;  procedures as arguments: a method for telling whether a guess is
;;;  good enough and a method for improving a guess. Iterative-improve
;;;  should return as its value a procedure that takes a guess as
;;;  argument and keeps improving the guess until it is good enough.
;;;  Rewrite the sqrt procedure of section 1.1.7 and the fixed-point
;;;  procedure of section 1.3.3 in terms of iterative-improve.

;;; Here are two versions of iterative-improve:

(define (iterative-improve good-enough? improve)
  (define (my-iterate-improve guess)
    (if (good-enough? guess)
        guess
        (my-iterate-improve (improve guess))))
  my-iterate-improve)

(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (if (good-enough? guess)
        guess
        ((iterative-improve good-enough? improve)
         (improve guess)))))

;;; The first one is clearer to me because it's clear what the
;;; procedure being returned is (my-iterate-improve). In the second,
;;; the procedure being returned is a lambda that calls iterative-improve
;;; itself, which returns a lambda that calls an iterative-improve that
;;; returns... Well, it gets confusing.

;;; fixed-point written with iterative-improve
;;; The improve function is f itself.

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  ((iterative-improve (lambda (g)
                        (< (abs (- g (f g))) tolerance))
                      f)
   first-guess))

;;; sqrt-iter written with iterative-improve

(define (average x y) (/ (+ x y) 2))
(define (sqrt-iter first-guess x)
  ((iterative-improve (lambda (g)
                        (< (abs (- (square g) x)) tolerance))
                      (lambda (g)
                        (average g (/ x g))))
   first-guess))

