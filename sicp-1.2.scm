;;--------------
;;EXERCISE 1.16.
;;--------------
;; Design a procedure that evolves an iterative
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

(define (square n) (* n n))

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

;;-------------
;;EXERCISE 1.17
;;-------------
;; The exponentiation algorithms in this section are based on
;; performing exponentiation by means of repeated multiplication. In a
;; similar way, one can perform integer multiplication by means of
;; repeated addition. The following multiplication procedure (in which
;; it is assumed that our language can only add, not multiply) is
;; analogous to the expt procedure:


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

;;-------------
;;EXERCISE 1.18
;;-------------
;; Using the results of exercises 1.16 and 1.17, devise a procedure
;; that generates an iterative process for multiplying two integers in
;; terms of adding, doubling, and halving and uses a logarithmic
;; number of steps.

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

;;-------------
;;EXERCISE 1.19
;;-------------
;; There is a clever algorithm for computing the
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
;;; Setting (p,q) = (0,1) (p',q') = (1,1)
;;; a <- 2a + b,  b <- a + b

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))   ; compute p'
                   (+ (* 2 p q) (* q q)) ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

;;---------------
;; EXERCISE 1.20.
;;---------------
;; The process that a procedure generates is of course
;; dependent on the rules used by the interpreter. As an example,
;; consider the iterative gcd procedure given above. Suppose we were
;; to interpret this procedure using normal-order evaluation, as
;; discussed in section 1.1.5. (The normal-order-evaluation rule for
;; if is described in exercise 1.5.) Using the substitution method
;; (for normal order), illustrate the process generated in evaluating
;; (gcd 206 40) and indicate the remainder operations that are
;; actually performed. How many remainder operations are actually
;; performed in the normal-order evaluation of (gcd 206 40)? In the
;; applicative-order evaluation?

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
;;                                     |   a |  b |  a | (= b 0) |
;;  Normal order: [r := remainder]   ; |-----+----+----+---------|
;; (gcd 206 40)                      ; | 206 | 40 |  0 |       0 | 
;; (gcd 40 (r 206 40))               ; |  40 |  6 |  0 |       1 |
;; (gcd (r 206 40)                   ; |     |    |    |         |
;;      (r 40 (r 206 40)))           ; |   6 |  4 |  0 |       2 |
;; (gcd (r 40 (r 206 40))            ; |     |    |    |         |
;;      (r (r 206 40)                ; |     |    |    |         |
;;         (r 40 (r 206 40))))       ; |   4 |  2 |  0 |       4 |
;; (gcd (r (r 206 40)                ; |     |    |    |         |
;;         (r 40 (r 206 40)))        ; |     |    |    |         |
;;      (r (r 40 (r 206 40))         ; |     |    |    |         |
;;         (r (r 206 40)             ; |     |    |    |         |
;;            (r 40 (r 206 40)))))   ; |   2 |  0 |  4 |       7 |
;;                                   ; |-----+----+----+---------|
;;                                   ; |     |    |  4 |      14 |
;;                                     |-----+----+----+---------|
;; 
;; The first two columns are the values of a & b (evaluated by me, not
;; scheme.) The third column is the number of r (remainder) calls in
;; the if statement's evaluation of 'a'. The last column is the number
;; of r calls in the evaluation of (= b 0). The sum of the numbers in
;; columns 3 & 4 is the total number of r calls. (+ 4 14): 18

;;; Applicative order:
;;  (gcd 206 40)
;;; (gcd 206 40)
;;; (gcd 40 (remainder 206 40))             ; 1
;;; (gcd 40 6)
;;; (gcd 6 (remainder 40 6))                ; 1
;;; (gcd 6 4)
;;; (gcd 4 (remainder 6 4))                 ; 1
;;; (gcd 4 2)                               
;;; (gcd 2 (remainder 4 2))                 ; 1
;;; (gcd 2 0)                               ; Done: 4 remainder ops


;;-------------
;;EXERCISE 1.21
;;-------------
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
;;--------------------------------

;;; To implement the Fermat test, we need a procedure that computes
;;; the exponential of a number modulo another number:

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;; (expmod 2 3 3)
;; (remainder (* 2 (expmod 2 2 3)) 3)
;; (remainder (* 2 (remainder (square (expmod 2 1 3)) 3)) 3)
;; (remainder (* 2 (remainder (square (remainder (* 2 (expmod 2 0 3)) 3)) 3)))
;; (remainder (* 2 (remainder (square (remainder (* 2 1) 3)) 3)) 3)
;; (remainder (* 2 (remainder (square 2) 3)) 3)
;; (remainder (* 2 (remainder 4 3)) 3)
;; (remainder (* 2 1) 3)
;; (remainder 2 3)
;; 2

;;-------------
;;EXERCISE 1.22
;;-------------
;;; Exercise 1.22. Most Lisp implementations include a primitive
;;; called runtime that returns an integer that specifies the amount
;;; of time the system has been running (measured, for example, in
;;; microseconds). The following timed-prime-test procedure, when
;;; called with an integer n, prints n and checks to see if n is
;;; prime. If n is prime, the procedure prints three asterisks
;;; followed by the amount of time used in performing the test.

;;; Note: I have modified timed-prime-test and report-prime to display
;;; only the primes found.

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      #f))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time)
  #t)

(define (search-for-primes from how-many)
  (cond ((= how-many 0) (newline) (display "done"))
        ((timed-prime-test from)
         (search-for-primes (+ from 1) (- how-many 1)))
        (else 
         (search-for-primes (+ from 1) how-many))))

;;; Anything below 1e7 returns a 0.0 process time
;; (search-for-primes 100000000 3)         ;0.02 secs
;; (search-for-primes 1000000000 3)        ;0.08 secs (0.063)
;; (search-for-primes 10000000000 3)       ;0.30 secs (0.25)
;; (search-for-primes 100000000000 3)      ;0.92 secs (0.95)
;; (search-for-primes 1000000000000 3)     ;2.9 secs  (2.91)

;;; For large numbers, an increase in a factor of 10 causes an
;;; increase in runtime of very close to (sqrt 10).

;;-------------
;;EXERCISE 1.23
;;-------------
;;; Exercise 1.23. The smallest-divisor procedure shown at the start
;;; of this section does lots of needless testing: After it checks to
;;; see if the number is divisible by 2 there is no point in checking
;;; to see if it is divisible by any larger even numbers. This
;;; suggests that the values used for test-divisor should not be
;;; 2,3,4,5,6,…, but rather 2,3,5,7,9,… To implement this change,
;;; define a procedure next that returns 3 if its input is equal to 2
;;; and otherwise returns its input plus 2. Modify the
;;; smallest-divisor procedure to use (next test-divisor) instead of
;;; (+ test-divisor 1). With timed-prime-test incorporating this
;;; modified version of smallest-divisor, run the test for each of the
;;; 12 primes found in exercise 1.22. Since this modification halves
;;; the number of test steps, you should expect it to run about twice
;;; as fast. Is this expectation confirmed? If not, what is the
;;; observed ratio of the speeds of the two algorithms, and how do you
;;; explain the fact that it is different from 2?

(define (next n)
  (if (= n 2) 3 (+ n 2)))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))

;;; Anything below 1e7 returns a 0.0 process time
;; (search-for-primes 100000000 3)         ;0.02 secs (no diff)
;; (search-for-primes 1000000000 3)        ;0.06 secs (0.063)
;; (search-for-primes 10000000000 3)       ;0.30 secs (0.25)
;; (search-for-primes 100000000000 3)      ;0.92 secs (0.95)
;; (search-for-primes 1000000000000 3)     ;2.9 secs  (2.91)

;;; New times
;; | Old time | New time | Old * sqrt 10 | New * sqrt 10 |
;; |     0.02 |     0.02 |               |               |
;; |     0.08 |     0.06 |         0.063 |         0.063 |
;; |     0.30 |     0.16 |          0.25 |         0.190 |
;; |     0.92 |     0.56 |          0.95 |         0.506 |
;; |      2.9 |     1.76 |          2.91 |          1.77 |
;; |          |          |               |               |

;;; The ratio between the speeds is (1, 1.33, 1.875, 1.64, 1.64) It
;;; appears to be approaching ~ 1.64. It's not 2 because the other
;;; operations (other cond clauses, divides?, etc) take the same time
;;; in both algorithms.

;;-------------
;;EXERCISE 1.24
;;-------------
;;; Modify the timed-prime-test procedure of exercise
;;; 1.22 to use fast-prime? (the Fermat method), and test each of the
;;; 12 primes you found in that exercise. Since the Fermat test has
;;; Θ(logn) growth, how would you expect the time to test primes near
;;; 1,000,000 to compare with the time needed to test primes near
;;; 1000? Do your data bear this out? Can you explain any discrepancy
;;; you find?

(define (start-prime-test n start-time)
  (if (fast-prime? n 1000)
      (report-prime n (- (runtime) start-time))
      #f))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;;; Anything below 1e7 returns a 0.0 process time
;; (search-for-primes 100000000 3)        ;0.10 (0.02 secs before)
;; (search-for-primes 1000000000 3)       ;0.12 (0.08 secs before)
;; (search-for-primes 10000000000 3)      ;0.16 (0.30 secs before)
;; (search-for-primes 100000000000 3)     ;0.18 (0.92 secs before)
;; (search-for-primes 1000000000000 3)    ;0.20 (2.90 secs before)

;;; Each factor of 10 increases the time by a constant, roughly 0.02
;;; seconds. This suggests the growth is logarithmic.

;;-------------
;;EXERCISE 1.25
;;-------------
;; Alyssa P. Hacker complains that we went to a lot of extra work in
;; writing expmod. After all, she says, since we already know how to
;; compute exponentials, we could have simply written

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

;;; Is she correct? Would this procedure serve as well for our fast
;;; prime tester? Explain.

;;; She's correct, it would work. It would be much slower,though.

;; (search-for-primes 100000000 3)        ;forever (0.02 secs before)
;; (search-for-primes 1000000000 3)       ;forever (0.08 secs before)
;; (search-for-primes 10000000000 3)      ;forever (0.30 secs before)
;; (search-for-primes 100000000000 3)     ;forever (0.92 secs before)
;; (search-for-primes 1000000000000 3)    ;forever (2.90 secs before)

;;; (fast-expt base exp) generates huge intermediate results.
;;; (x^10000000), etc

;;-------------
;;EXERCISE 1.26
;;-------------

;; Louis Reasoner is having great difficulty doing exercise 1.24. His
;; fast-prime? test seems to run more slowly than his prime? test.
;; Louis calls his friend Eva Lu Ator over to help. When they examine
;; Louis's code, they find that he has rewritten the expmod procedure
;; to use an explicit multiplication, rather than calling square:

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;; “I don't see what difference that could make,” says Louis. “I do.”
;; says Eva. “By writing the procedure like that, you have transformed
;; the Θ(logn) process into a Θ(n) process.” Explain.

;; Scheme uses applicative order evaluation, so 

;; (* (expmod base (/ exp 2) m) (expmod base (/ exp 2) m)) 

;; is two calls to expmod. Square makes only one call. (expmod base (/
;; exp 2) m) is computed twice at each step instead of once, so the
;; problem size is not halved. Also, The process changes from a linear
;; recursion to a tree recursion, whose execution time is proportional
;; to the number of tree leaf nodes. This makes the problem O(n).

;;-------------
;;EXERCISE 1.27
;;-------------
;; Demonstrate that the Carmichael numbers listed in Footnote 1.47
;; really do fool the Fermat test. That is, write a procedure that
;; takes an integer n and tests whether a^n is congruent to a modulo n
;; for every a < n, and try your procedure on the given Carmichael
;; numbers.

(define carmichael '(561 1105 1729 2465 2821 6601))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test-full n a)
  (= (expmod a n n) a))

(define (fast-prime-full? n times)
  (cond ((= times 0) true)
        ((fermat-test-full n times) (fast-prime-full? n (- times 1)))
        (else false)))

(define (prime-full? n)
  (fast-prime-full? n (- n 1)))

(map prime-full? carmichael)            ;(#t #t #t #t #t #t)
(map prime? carmichael)                 ;(#f #f #f #f #f #f)
;;; So the carmichael numbers do fool the fermat test.

;;-------------
;;EXERCISE 1.28
;;-------------
;; One variant of the Fermat test that cannot be fooled is called the
;; Miller-Rabin test (Miller 1976; Rabin 1980). This starts from an
;; alternate form of Fermat's Little Theorem, which states that if n
;; is a prime number and a is any positive integer less than n, then a
;; raised to the (n - 1)st power is congruent to 1 modulo n. To test
;; the primality of a number n by the Miller-Rabin test, we pick a
;; random number a < n and raise a to the (n - 1)st power modulo n
;; using the expmod procedure. However, whenever we perform the
;; squaring step in expmod, we check to see if we have discovered a
;; “nontrivial square root of 1 modulo n,” that is, a number not equal
;; to 1 or n - 1 whose square is equal to 1 modulo n. It is possible
;; to prove that if such a nontrivial square root of 1 exists, then n
;; is not prime. It is also possible to prove that if n is an odd
;; number that is not prime, then, for at least half the numbers a <
;; n, computing a^(n-1) in this way will reveal a nontrivial square root
;; of 1 modulo n. (This is why the Miller-Rabin test cannot be
;; fooled.) Modify the expmod procedure to signal if it discovers a
;; nontrivial square root of 1, and use this to implement the
;; Miller-Rabin test with a procedure analogous to fermat-test. Check
;; your procedure by testing various known primes and non-primes.

;; Hint: One convenient way to make expmod signal is to have it return
;; 0.

;; UNSOLVED. I DON'T UNDERSTAND THE QUESTION: "However, whenever we
;; perform the squaring step in expmod, we check to see if we have
;; discovered a “nontrivial square root of 1 modulo n,” that is, a
;; number not equal to 1 or n - 1 whose square is equal to 1 modulo n."

;; (define (expmod base exp m)
;;   (cond ((= exp 0) 1)
;;         ((even? exp)
;;          (remainder (square (expmod base (/ exp 2) m))
;;                     m))
;;         (else
;;          (remainder (* base (expmod base (- exp 1) m))
;;                     m))))

;; (and (not (= a 1))
;;      (not (= a (- n 1)))
;;      (= (expmod a 2 n) 1))

;; (define (miller-fermat-test n)
;;   (define (try-it a)
;;     (= (expmod a n n) a))
;;   (try-it (+ 1 (random (- n 1)))))

;; (define (miller-fast-prime? n times)
;;   (cond ((= times 0) true)
;;         ((miller-fermat-test n) (miller-fast-prime? n (- times 1)))
;;         (else false)))

;; (expmod 39 40 41)
;; (define carmichael '(561 1105 1729 2465 2821 6601))

