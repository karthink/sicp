;;----------------------------------------------------
;;THE Y-COMBINATOR, A DERIVATION WITH HELPFUL EXAMPLES
;;----------------------------------------------------
;;; Transcribed from Jim Weirich's talk at Ruby Conf 2012
;;; https://www.youtube.com/watch?v=FITJMJjASUs

;;; This document assumes you have passing familiarity with lambda
;;; expressions and Scheme and have heard of lambda calculus.

;;; Let's start with simple lambda expressions
(define add1 (lambda (n) (+ n 1)))
(define mul3 (lambda (n) (* n 3)))

(mul3 (add1 10))

;;; A couple of higher-order function
(define make-adder (lambda (x) (lambda (n) (+ n x))))
(define add1 (make-adder 1))

(define compose (lambda (f g) (lambda (n) (f (g n)))))
(define mul3add1 (compose mul3 add1))
(mul3add1 10)

;;-----------------------
;;FUNCTIONAL REFACTORINGS
;;-----------------------
;;; We will be using these to manipulate lambda expressions. These are
;;; fairly simple rules from lambda calculus.

;;; (1) Tennent Correspondence Principle
;;; (2) Introduce Binding
;;; (3) Wrap Function
;;; (4) Inline Definition

;;--------------------------------
;;TENNENT CORRESPONDENCE PRINCIPLE
;;--------------------------------
;;; Says x === ((lambda () x))

;;; From
(define compose (lambda (f g)
                  (lambda (n) (f (g n)))
                  ))
;;; To
(define compose (lambda (f g)
                  ((lambda () (lambda (n) (f (g n)))))
                  ))

;;-----------------
;;INTRODUCE BINDING
;;-----------------
;;; Says x === ((lambda (y) x) 'any-y-value)

;;; From 
(define compose (lambda (f g)
                  (lambda (n) (f (g n)))
                  ))
;;; To
(define compose (lambda (f g)
                  ((lambda (y) (lambda (n) (f (g n)))) 1234567)
                  ))

;;----------------
;;WRAP FUNCTION
;;----------------
;;; Says (lambda (x) (body)) === (lambda (y) ((lambda (x) (body)) y))
;;; Or   f              === (lambda (y) (f y))

;;; From
(define make-adder (lambda (x) (+ n x)))

;;; To
(define make-adder (lambda (y)
                     (lambda (x) (+ n x))
                     y))

;;-----------------
;;INLINE DEFINITION
;;-----------------
;;; Says (define f (lambda (x) body)); (f y)
;;; ===  ((lambda (x) body) y)

;;; From
(define make-adder (lambda (x) (lambda (n) (+ n x))))
(make-adder 1)
;;; To
((lambda (x) (lambda (n) (+ n x))) 1)

;;; Going crazy with inline definition

;; (define make-adder (lambda (x) (lambda (n) (+ n x))))
;; (define add1 (make-adder 1))
;; (define mul3 (lambda (n) (* n 3)))
;; (define compose (lambda (f g) (lambda (n) (f (g n)))))

;;; The following expressions are equivalent

(mul3 (add1 10))

((compose (mul3 add1)) 10)

(((lambda (f g) (lambda (n) (f (g n))))
  (lambda (n) (* n 3))
  ((lambda (x) (lambda (n) (+ n x))) 1))
 10)

;;; This was effective calculation using only lambda expressions

;;---------
;;FACTORIAL
;;---------
;;; One apparent limitation of lambda calculus (and anonymous functions) is
;;; that With lambda bindings alone, you cannot make recursive functions:

(define fact (lambda (n) (if (= 0 n) 1 (* n (fact (- n 1))))))

;;; because all functions must be anonymous.
;;; But you can have names as _variable_ bindings. So let us:

(define make-fact (lambda (fact)
                    (lambda (n)
                      (if (= 0 n)
                          1
                          (* n (fact (- n 1)))))))

;;; Now that we can use 'fact' legitimately, all we need is its
;;; definition:

;;; (define factorial (make-fact 'definition-of-factorial))

;;; This doesn't solve anything, because (of course) it's circular.
;;; Let's convert make-fact to a fact-improver instead.

(define fact-improver (lambda (partial)
                        (lambda (n)
                          (if (= 0 n)
                              1
                              (* n (partial (- n 1)))))))

;;; partial is a factorial function that works on a subset of all its
;;; possible inputs.

;;; If (partial n) returns n! for n between 0 and 10, (fact-improver
;;; partial) will return a function that works for n between 0 and 11
;;; = (+ 10 1).

(define err (lambda (n) (error "SHOULD NEVER BE CALLED")))
(define f0 (fact-improver err)) 

;;; f0 will return (factorial n) for n = 0... and nothing else.
;;; But wait. We can improve upon f0 using fact-improve:

(define f1 (fact-improver f0))

;;; (f1 1) works... (f1 2) does not.

(f1 2)           ;-> (* 2 (f0 1)) => (* 2 (* 1 (err 0)))

;;; But we can continue the chain
(define f2 (fact-improver f1))
(define f3 (fact-improver f2))
(define f4 (fact-improver f3))
(define f5 (fact-improver f4))

;;; The definition of f3 is
(define f3 (fact-improver
            (fact-improver
             (fact-improver
              (fact-improver
               (fact-improver err))))))

;;; Let's refactor fact-improver

(define f1 (fact-improver (fact-improver err)))

;;; into

(define f1 ((lambda (dummy-improver)
              (dummy-improver (dummy-improver err)))
            (lambda (partial)
              (lambda (n)
                (if (= 0 n)
                    1
                    (* n (partial (- n 1))))))))

;;; Using the wrap-function refactoring.

;;; fact-improver becomes a dummy variable and its procedure is passed
;;; the definition of fact-improver

;;; But err's got nothing to do with fact, really. Can we get rid of it?

(define f1 ((lambda (dummy-improver)
              (dummy-improver dummy-improver))
            (lambda (partial)
              (lambda (n)
                (if (= 0 n)
                    1
                    (* n (partial (- n 1))))))))


;;; No.

;;; Now it won't work for n = 1 anymore

;;; (It tries (* 1 (partial (- n 1))))

;;; where partial is dummy-improver, which expects a function
;;; argument. (- n 1) is not a function.

;;; Let's rewrite f1 with improver instead of partial so this is
;;; clearer.

(define f1 ((lambda (dummy-improver)
              (dummy-improver dummy-improver))
            (lambda (improver)
              (lambda (n)
                (if (= 0 n)
                    1
                    (* n (improver (- n 1))))))))

;;; Now, let's try to get rid of that error by changing the last line
;;; of the body of the fact-improver function,
;;; from (improver (- n 1)) to ((improver improver) (- n 1))

(define fx ((lambda (dummy-improver)
              (dummy-improver dummy-improver))
            (lambda (improver)
              (lambda (n)
                (if (= 0 n)
                    1
                    (* n ((improver improver) (- n 1))))))))

;;; A surprising thing happens

(fx 1)                                  ;1
(fx 2)                                  ;2
(fx 5)                                  ;120
(fx 10)                                 ;3628800
(fx 100)                                ;93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000

;;;  :-/
;;;  WAT.

;;; We have inadvertently defined a factorial function. 

;;; 1. "improver" is no longer appropriate. Let's change it to gen,
;;; for generator

(define fx ((lambda (dummy-gen)
              (dummy-gen dummy-gen))
            (lambda (gen)
              (lambda (n)
                (if (= 0 n)
                    1
                    (* n ((gen gen) (- n 1))))))))

;;; or 

(define fx ((lambda (gen)
              (gen gen))
            (lambda (gen)
              (lambda (n)
                (if (= 0 n)
                    1
                    (* n ((gen gen) (- n 1))))))))

;;; To simplify, let's do (1) Tennent refactoring on (lambda (n) (...))
;;; (Put it in the body of an anonymous no-argument function and call it)
;;; (lambda (n) (...)) => ((lambda () (lambda (n) (...))))

(define fx ((lambda (gen)
              (gen gen))
            (lambda (gen)
              ((lambda ()
                 (lambda (n)
                   (if (= 0 n)
                       1
                       (* n ((gen gen) (- n 1)))))))
              )))

;;; Next let's introduce a binding. the anonymous no-argument function
;;; now accepts one argument, code, and is called with err from
;;; before. It changes nothing, of course, because code is never
;;; called.

(define fx ((lambda (gen)
              (gen gen))
            (lambda (gen)
              ((lambda (code)
                 (lambda (n)
                   (if (= 0 n)
                       1
                       (* n ((gen gen) (- n 1)))))) err)
              )))

;;; If we can pass anything as code, let's just pass in gen.

(define fx ((lambda (gen)
              (gen gen))
            (lambda (gen)
              ((lambda (code)
                 (lambda (n)
                   (if (= 0 n)
                       1
                       (* n ((gen gen) (- n 1)))))) gen)
              )))

;;; If we can pass gen, why not (gen gen)?

(define fx ((lambda (gen)
              (gen gen))
            (lambda (gen)
              ((lambda (code)
                 (lambda (n)
                   (if (= 0 n)
                       1
                       (* n ((gen gen) (- n 1)))))) (gen gen))
              )))

;;; ;Aborting!: maximum recursion depth exceeded Why? Because (gen
;;; gen) (in place of err) is evaluated first, and it bottoms out. So
;;; let's delay its evaluation by wrapping it in a function
;;; (Refactoring (3)).

(define fx ((lambda (gen)
              (gen gen))
            (lambda (gen)
              ((lambda (code)
                 (lambda (n)
                   (if (= 0 n)
                       1
                       (* n ((gen gen) (- n 1)))))) (lambda (v) ((gen gen) v)))
              )))

(fx 10)

;;; Works again.
;;; More wrapping, this time in the body of fact-improver

(define fx ((lambda (gen)
              (gen gen))
            (lambda (gen)
              ((lambda (code)
                 (lambda (n)
                   (if (= 0 n)
                       1
                       (* n ((lambda (v) ((gen gen) v)) (- n 1))))))
               (lambda (v) ((gen gen) v)))
              )))

(fx 10)

;;; Yup. But (lambda (v) ((gen gen) v)) is the exact same thing as code. So let's call code instead.

(define fx ((lambda (gen)
              (gen gen))
            (lambda (gen)
              ((lambda (code)
                 (lambda (n)
                   (if (= 0 n)
                       1
                       (* n (code (- n 1))))))
               (lambda (v) ((gen gen) v)))
              )))

;;; Let's rename code to partial:

(define fx ((lambda (gen)
              (gen gen))
            (lambda (gen)
              ((lambda (partial)                       ; This is fact-improver!
                 (lambda (n)                           ;
                   (if (= 0 n)                    ;
                       1                          ;
                       (* n (partial (- n 1)))))) ;
               (lambda (v) ((gen gen) v)))
              )))

;;; One more tennent refactoring, wrapping it in a lambda

(define fx ((lambda () ((lambda (gen)
                     (gen gen))
                   (lambda (gen)
                     ((lambda (partial)                       ; This is fact-improver!
                        (lambda (n)                           ;
                          (if (= 0 n)                    ;
                              1                          ;
                              (* n (partial (- n 1)))))) ;
                      (lambda (v) ((gen gen) v)))
                     )))))

;;; Repeat: Introduce binding, variable code and call with error

(define fx ((lambda (code) ((lambda (gen)
                     (gen gen))
                   (lambda (gen)
                     ((lambda (partial)                       ; This is fact-improver!
                        (lambda (n)                           ;
                          (if (= 0 n)                    ;
                              1                          ;
                              (* n (partial (- n 1)))))) ;
                      (lambda (v) ((gen gen) v)))
                     ))) error))

;;; Now we pass in the body of fact-improver as the value of code (in
;;; place of error)

(define fx ((lambda (code) ((lambda (gen)
                     (gen gen))
                   (lambda (gen)
                     ((lambda (partial)                       ; This is fact-improver!
                        (lambda (n)                           ;
                          (if (= 0 n)                    ;
                              1                          ;
                              (* n (partial (- n 1)))))) ;
                      (lambda (v) ((gen gen) v)))
                     ))) (lambda (partial)                    ; This is fact-improver!
                        (lambda (n)                           ;
                          (if (= 0 n)                    ;
                              1                          ;
                              (* n (partial (- n 1))))))))

;;; Yeah, it's basically unreadable at this point. But I just replaced
;;; error with

(lambda (partial)                       ; This is fact-improver!
  (lambda (n)                           ;
    (if (= 0 n)                    ;
        1                          ;
        (* n (partial (- n 1)))))) ;

;;; Same trick again now. fact-improver in the body of (lambda (code) ...)
;;; and as code itself at the end are the same. So let's replace it
;;; with code.

(define fx ((lambda (code) ((lambda (gen)
                     (gen gen))
                   (lambda (gen)
                     (code (lambda (v) ((gen gen) v))))))
            (lambda (partial)                ; This is fact-improver!
              (lambda (n)                    ;
                (if (= 0 n)             ;
                    1                   ;
                    (* n (partial (- n 1))))))))

;;; Now rename code as improver

(define fx ((lambda (improver) ((lambda (gen)
                     (gen gen))
                   (lambda (gen)
                     (improver (lambda (v) ((gen gen) v))))))
            (lambda (partial)                ; This is fact-improver!
              (lambda (n)                    ;
                (if (= 0 n)             ;
                    1                   ;
                    (* n (partial (- n 1))))))))

;;; There are two pieces to this. The lambda that takes improver as its
;;; argument, and the lambda that improver binds to in the call. We can
;;; separate these two and write:

(define fact-improver (lambda (partial)                ; This is fact-improver!
                        (lambda (n)                    ;
                          (if (= 0 n)             ;
                              1                   ;
                              (* n (partial (- n 1)))))))

(define y (lambda (improver) ((lambda (gen) (gen gen))
                         (lambda (gen) (improver (lambda (v) ((gen gen) v)))))))

(define fx (y fact-improver))
(fx 10)                                 ; Still works

;;; Okay, now let's think about what's happening:
;;; 1. fx is the fact function.

;;; What does this give?
;;; (fact-improver fx) ???
;;; Well, it's still fx:

(define fx (fact-improver fx))
(fx 10)                                 ; Same

;;; 2. So fx is the fixed-point of fact-improver
;;; 3. So y calculates the fixed-point of an improver
;;; 4. y is the Y-COMBINATOR

;;; Usually written as:

(define y (lambda (f) ((lambda (x) (x x))
                  (lambda (x) (f (lambda (v) ((x x) v)))))))

;;; (with improver => f, gen => x)
;;; Or: 

(define y (lambda (f) ((lambda (x) (f (x x)))
                  (lambda (x) (f (lambda (v) ((x x) v)))))))

;;; With a function wrap:

(define y (lambda (f) ((lambda (x) (f (lambda (v) ((x x) v))))
                  (lambda (x) (f (lambda (v) ((x x) v)))))))

;;; (x x) becomes (f (x x)). Doesn't matter because f is an improver.

;;; This is the applicative-order y-combinator.
;;; Or the z-combinator
;;; Or the fixpoint-combinator

(define y (lambda (improver) ((lambda (gen) (improver (lambda (v) ((gen gen) v))))
                         (lambda (gen) (improver (lambda (v) ((gen gen) v)))))))

;;----
;;TEST
;;----
;;; Let's write an improver function for the fibonacci series:
(define fib-improver (lambda (partial)
                        (lambda (n)
                          (if (or (= 0 n) (= 1 n))
                              n
                              (+ (partial (- n 1))
                                 (partial (- n 2)))))))

((fib-improver err) 2)                  ;"THIS SHOULD NEVER BE CALLED"

((fib-improver
  (fib-improver err)) 2)                ;1

((fib-improver
  (fib-improver
   (fib-improver err))) 3)              ;2


;;; and so on.

;;; Now the magic:
(define fib (y fib-improver))

(fib 7)                                 ;13
(fib 14)                                ;377
(fib 26)                                ;121393

;;; And that's all, folks.



              
