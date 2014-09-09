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
