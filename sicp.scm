;; Exercise 1.16. Design a procedure that evolves an iterative
;; exponentiation process that uses successive squaring and uses a
;; logarithmic number of steps, as does fast-expt. (Hint: Using the
;; observation that (b^n/2)^2=(b^2)^n/2, keep, along with the exponent
;; n and the base b, an additional state variable a, and define the
;; state transformation in such a way that the product ab^n is
;; unchanged from state to state. At the beginning of the process a is
;; taken to be 1, and the answer is given by the value of a at the end
;; of the process. In general, the technique of defining an invariant
;; quantity that remains unchanged from state to state is a powerful
;; way to think about the design of iterative algorithms.)

(define (fast-expt b n)
  (fast-expt-iter b n 1))

(define (even? n)
  (= (remainder n 2) 0 ))

(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (square b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* a b)))))

;; State: b n a
;;;  b n a
;;;  3 5 1   ab^n = 243
;;;  3 4 3   ab^n = 243
;;;  9 2 3   ab^n = 243
;;; 81 1 3   ab^n = 243
;;; 81 0 243 ab^n = 243

;; Exercise 1.17. The exponentiation algorithms in this section are
;; based on performing exponentiation by means of repeated
;; multiplication. In a similar way, one can perform integer
;; multiplication by means of repeated addition. The following
;; multiplication procedure (in which it is assumed that our language
;; can only add, not multiply) is analogous to the expt procedure:


(define (mulf a b)
  (if (= b 0)
      0
      (+ a (mulf a (- b 1)))))

;; This algorithm takes a number of steps that is linear in b. Now
;; suppose we include, together with addition, operations double,
;; which doubles an integer, and halve, which divides an (even)
;; integer by 2. Using these, design a multiplication procedure
;; analogous to fast-expt that uses a logarithmic number of steps.

(define (halve  n) (/ n 2))
(define (double n) (+ n n))

(define (mul a b)
  (cond ((= a 1) b)
        ((even? a) (mul (halve a) (double b)))
        (else (+ b (mul (- a 1) b)))))

;;--------------------------------
;; Exercise 1.18. Using the results of exercises 1.16 and 1.17, devise
;; a procedure that generates an iterative process for multiplying two
;; integers in terms of adding, doubling, and halving and uses a
;; logarithmic number of steps.

(define (mul a b)
  (mul-iter a b 0))

(define (mul-iter a b prod)
  (cond ((= a 0) prod)
        ((even? a) (mul-iter (halve a) (double b) prod))
        (else (mul-iter (- a 1) b (+ prod b)))))

;;; prod + ab = const
;;;   a  b  prod
;;;  15  4  0
;;;  14  4  (+ 0 4)
;;;   7  8  (+ 0 4)
;;;   6  8  (+ 0 4 8)
;;;   3 16  (+ 0 4 8)
;;;   2 16  (+ 0 4 8 16)
;;;   1 32  (+ 0 4 8 16 32)

;;--------------------------------
;; Exercise 1.19. There is a clever algorithm for computing the
;; Fibonacci numbers in a logarithmic number of steps. Recall the
;; transformation of the state variables a and b in the fib-iter
;; process of section 1.2.2:

;; a <- a+b
;; b <- a

;; Call this transformation T, and observe that applying T over and
;; over again n times, starting with 1 and 0, produces the pair Fib(n
;; + 1) and Fib(n). In other words, the Fibonacci numbers are produced
;; by applying Tn, the nth power of the transformation T, starting
;; with the pair (1,0). Now consider T to be the special case of p = 0
;; and q = 1 in a family of transformations Tpq, where Tpq transforms
;; the pair (a,b) according to

;; a <- bq+aq+ap
;; b <- bp+aq

;; Show that if we apply such a transformation Tpq twice, the effect
;; is the same as using a single transformation Tp′q′ of the same
;; form, and compute p′ and q′ in terms of p and q. This gives us an
;; explicit way to square these transformations, and thus we can
;; compute Tn using successive squaring, as in the fast-expt
;; procedure. Put this all together to complete the following
;; procedure, which runs in a logarithmic number of steps:

;;; Apply Tpq twice:
;;; a <- bq + aq + ap
;;; b <- bp + aq
;;; Gives p' = p^2 + q^2, q' = q^2 + 2 p q 
;;; Setting (p,q) = (0,1)
;;; (p',q') = (1,1)

