(load "sicp-2.4-helper.scm")
(load "sicp-2.5.3-helper.scm")

;;--------------
;;EXERCISE 2.87:
;;--------------

;;; Install =zero? for polynomials in the generic arithmetic package.
;;; This will allow adjoin-term to work for polynomials with
;;; coefficients that are themselves polynomials.

;;; In the polynomial package
(put '=zero? '(polynomial)
     (lambda (p) (empty-termlist? (term-list p))))

;;--------------
;;EXERCISE 2.88:
;;--------------

;;; Extend the polynomial system to include subtraction of
;;; polynomials. (Hint: You may find it helpful to define a generic
;;; negation operation.)

;;; Using a negation operation 'negate',
;;; In the polynomial package:
(define (negate-poly p)
  (if (empty-termlist? (term-list p))
      p
      (let (t1 (first-term p))
        (adjoin-term (make-term (order t1) (negate (coeff t1)))
                     (negate (make-poly (variable p)
                                        (rest-terms p)))))))
(put 'negate '(polynomial) (lambda (p) (tag (negate-poly p))))
(put 'sub '(polynomial polynomial)
     (lambda (p1 p2) (tag (add-poly p1 (negate-poly p2)))))

;;; Note: To handle coefficients correctly, this will require negate
;;; to be installed in all packages (complex, real, rational, integer)
;;; For example:
;;; In the rectangular complex number package
(put 'negate '(rectangular) (lambda (z) (tag  (make-from-real-imag
                                      (- (real-part z))
                                      (- (imag-part z))))))
;;; In the polar complex number package
(put 'negate '(polar) (lambda (z) (tag (make-from-mag-ang
                                   (magnitude z)
                                   (+ (angle z) 3.141592653)))))
;;; In the complex number package
(put 'negate '(complex) (lambda (z) (tag (negate z))))

;;; (Alternatively, just this in the complex package works:)
;;; (put 'negate '(complex)
;;;      (lambda (z) (tag (sub-complex (make-from-real-imag 0 0) z))))

;;; Outside all packages:
(define (negate x) (apply-generic 'negate x))
;;; These additions have been made to sicp-2.4-helper.scm

;;--------------
;;EXERCISE 2.89:
;;--------------

;;; Define procedures that implement the term-list representation
;;; described above as appropriate for dense polynomials.

;;; x^4 + 3 x^2 - 4x + 2 => (1 0 3 -4 2)

;;; Only one procedure, first-term, needs to be changed. This is
;;; because first-term is the only procedure that extracts a term from
;;; the term-list for operation by other procedures. 

(define (the-empty-termlist) '())
(define (first-term term-list) (cons (- (length term-list) 1)
                                     (car term-list)))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))
(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

;;--------------
;;EXERCISE 2.90:
;;--------------

;;; Suppose we want to have a polynomial system that is efficient for
;;; both sparse and dense polynomials. One way to do this is to allow
;;; both kinds of term-list representations in our system. The
;;; situation is analogous to the complex-number example of Section
;;; 2.4, where we allowed both rectangular and polar representations.
;;; To do this we must distinguish different types of term lists and
;;; make the operations on term lists generic. Redesign the polynomial
;;; system to implement this generalization. This is a major effort,
;;; not a local change.

;;; Steps involved: 

;;; 1. Create two new packages and tags: sparse-terms and dense-terms. 

;;; 2. Put the corresponding term operations into install-sparse-terms
;;; and install-dense-terms respectively. Remove them from
;;; install-polynomial package.

;;; 3. install these operations (with tags) in the operator table.

(define (install-sparse-terms)
  
  ;; Internal representation of terms
  (define (adjoin-term-sparse term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  
  ;; Interface to rest of system
  (define (tag t) (attach-tag 'sparse p))
  (put 'make-term 'sparse (lambda (order coeff) (tag (list order coeff))))
  (put 'order '(sparse) car)
  (put 'coeff '(sparse) cadr)
  (put 'adjoin-term '(sparse sparse)
      (lambda (t t-list) (tag (adjoin-term-sparse t t-list))))
  ;;(put 'the-empty-termlist '(sparse) (lambda () '()))
  (put 'empty-termlist? '(sparse) (lambda (t-list) (null? t-list)))
  (put 'first-term '(sparse) (lambda (t-list) (tag (car t-list))))
  (put 'rest-terms '(sparse) (lambda (t-list) (tag (cdr t-list))))
  
  )

(define (install-dense-terms)
  
  ;; Internal representation of terms
  
  (define (adjoin-term-dense term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  
    ;; Interface to rest of system
  (define (tag t) (attach-tag 'dense p))
  (put 'make-term 'dense (lambda (order coeff) (tag (list order coeff))))
  (put 'order '(dense) car)
  (put 'coeff '(dense) cadr)
  (put 'adjoin-term '(dense dense)
       (lambda (t t-list) (tag (adjoin-term-dense t t-list))))
  ;;(put 'the-empty-termlist '(dense) (lambda () '()))
  (put 'empty-termlist? '(dense) (lambda (t-list) (null? t-list)))
  (put 'first-term '(dense)
       (lambda (t-list) (tag (cons (- (length term-list) 1)
                              (car term-list)))))
  (put 'rest-terms '(dense) (lambda (t-list) (tag (cdr t-list))))
  
  )

(define (order term) (apply-generic 'order term))
(define (coeff term) (apply-generic 'coeff term))
(define (adjoin-term term term-list) (apply-generic 'adjoin-term term term-list))
(define (empty-termlist? term-list) (apply-generic 'empty-termlist? term-list))
(define (first-term term-list) (apply-generic 'first-term term-list))
(define (rest-terms term-list) (apply-generic 'rest-terms term-list))

;;; In install-polynomial-package, we add
(define (make-sparse-term order coeff)
  ((get 'make-term 'sparse) order coeff))
(define (make-dense-term order coeff)
  ((get 'make-term 'dense) order coeff))

;;; INCOMPLETE.

;;--------------
;;EXERCISE 2.91:
;;--------------

;;; A univariate polynomial can be divided by another one to produce a
;;; polynomial quotient and a poly- nomial remainder. For example,

;;; x^5 - 1
;;; ------ = = x^3 + x , remainder x - 1.
;;; x^2 - 1

;;; Division can be performed via long division. that is, divide the
;;; highest-order term of the dividend by the highest-order term of
;;; the divisor. The result is the first term of the quotient. Next,
;;; multiply the result by the divisor, subtract that from the
;;; dividend, and produce the rest of the answer by recursively
;;; dividing the difference by the divisor. Stop when the order of the
;;; divisor exceeds the order of the dividend and declare the dividend
;;; to be the remainder. Also, if the dividend ever becomes zero,
;;; return zero as both quotient and remainder.

;;; We can design a div-poly procedure on the model of add-poly and
;;; mul-poly. The procedure checks to see if the two polys have the
;;; same variable. If so, div-poly strips off the variable and passes
;;; the problem to div-terms, which performs the division operation
;;; on term lists. Div-poly finally reattaches the variable to the
;;; result supplied by div-terms. It is convenient to design div-terms
;;; to compute both the quotient and the remainder of a division.
;;; Div-terms can take two term lists as arguments and return a list
;;; of the quotient term list and the remainder term list.

;;; Complete the following definition of div-terms by filling in the
;;; missing expressions. Use this to implement div-poly,which takes
;;; two polys as arguments and returns a list of the quotient and
;;; remainder polys.

(define (sub-terms L1 L2) (add-terms L1 (negate-terms L2)))
(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result
                     ;; compute rest of result recursively
                     (div-terms (sub-terms
                                 L1
                                 (mul-term-by-all-terms (make-term new-o new-c)
                                                        L2))
                                L2)))
                ;; form complete result
                (list (adjoin-term (make-term new-o new-c)
                                   (car rest-of-result))
                      (cadr rest-of-result))))))))

(define (div-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (let ((quotient-and-remainder (div-terms (term-list p1) (term-list p2))))
        (list (make-poly (variable p1)
                         (car quotient-and-remainder))
              (make-poly (variable p1)
                         (cadr quotient-and-remainder))))
      (error "Polys not in same var: DIV-POLY" (list p1 p2))))

(put 'div '(polynomial polynomial)
       (lambda (p1 p2) (tag (div-poly p1 p2))))

;;--------------
;;EXERCISE 2.92:
;;--------------

;;; By imposing an ordering on variables, extend the polynomial
;;; package so that addition and multiplication of polynomials works
;;; for polynomials in different variables. (This is not easy!)

;;; Not touching this right now because "This is not easy!"

;;; Here's the scaffolding though. We need:

;;; 1. Some way to (recursively) convert a mixed-variable polynomial
;;; into a single variable one with the other variables buried in the
;;; coefficients.

;;; Ex: 3 x^4 y - 5 x^2 y^2 - 2 x^2 y + 6 x^2 y^3 + 4 x y - 6 y
;;;     becomes
;;;     p1 = (3 y) x^4 + (6 y^3 - 5 y^2 - 2 y) x^2 + (4 y) x - 6 y

;;; 2. Some way to change (switch) the grouping variable, so that (for
;;; ex.) the above polynomial is converted to:

;;;     p2 = (group (term-list p1) 'y)
;;;     p2 = (6 x^2) y^3 + (-5 x^2) y^2 + ( 3 x^4 - 2 x^2 + 4 x - 6) y

(define (group poly var)
  (collect (expand poly) var))

;;; Collect and expand require a suitable datastructure
;;; poly example: (polynomial 'x '((3 (polynomial 'y '((4 2) (2 3))))
;;;                                (2 (polynomial 'y '((4 -1) (2 2)))))
;;; Expand will create (recursively):
;;;
;;;(polynomial 'x '((3 (polynomial 'y '((4 2))))
;;;                 (3 (polynomial 'y '((2 3))))
;;;                 (2 (polynomial 'y '((4 -1))))
;;;                 (2 (polynomial 'y '((2 2))))))

;;; and return:
;;; (list (polynomial 'x '((3 (polynomial 'y '((4 2))))))
;;;       (polynomial 'x '((3 (polynomial 'y '((2 3))))))
;;;       ...)

;;; Then collect will recursively traverse each entry in this list
;;; looking for polynomials in var. Since each of these terms can be
;;; written with the variables in any order, i. e.

;;; (polynomial 'x '((3 (polynomial 'y '((4 2))))))
;;; ==
;;; (polynomial 'y '((4 (polynomial 'x '((3 2))))))

;;; More generally:
;;; (polynomial 'x '((3 (polynomial 'y '((4 2))))
;;;                  (3 (polynomial 'y '((2 3))))
;;;                  (2 (polynomial 'y '((4 -1))))
;;;                  (2 (polynomial 'y '((2 2))))))
;;; (polynomial 'x '((a (polynomial 'y '((b anything))))))
;;; ==
;;; (polynomial 'y '((b (polynomial 'x '((a anything))))))

;;; So we can pull the polynomial of variable var to the front and
;;; bury the rest. We do this with every term in the list and smash
;;; together common powers of (here) y. Then we stick them all into
;;; one polynomial of y.

;;; (polynomial 'x '((3 something) (2 something-else)))
;;; should become:
;;; ((polynomial 'x '((3 something)))
;;;  (polynomial 'x '((2 something-else))))

;; (polynomial 'x '((3 (polynomial 'y '((4 2))))  
;;                 (3 (polynomial 'y '((2 3))))  
;;                 (2 (polynomial 'y '((4 -1)))) 
;;                 (2 (polynomial 'y '((2 2))))))

(define (expand-poly poly)
    (let ((terms (term-list poly)))
      (if (empty-termlist? terms)
          nil
          (let ((first (first-term terms))
                (rest  (rest-terms terms ))
                (var   (variable poly)))
            (append (map (lambda (c)
                               (make-polynomial var
                                                (adjoin-term
                                                 (make-term (order first)
                                                            c)
                                                 (the-empty-termlist))))
                             (expand (coeff first)))
                    (expand (make-polynomial var rest)))))))

;; ((polynomial x (3 (polynomial y (4 2))))
;;  (polynomial x (3 (polynomial y (2 3))))
;;  (polynomial x (2 (polynomial y (4 -1))))
;;  (polynomial x (2 (polynomial y (2 2))))
;;  (polynomial x (3 (polynomial y (1 -5))))
;;  (polynomial x (3 (polynomial y (2 3))))
;;  (polynomial x (2 (polynomial y (4 -1))))
;;  (polynomial x (2 (polynomial y (2 2)))))

;;; Convert 
;;; (polynomial 'x '((a (polynomial 'y '((b anything))))))
;;; to
;;; (polynomial 'y '((b (polynomial 'x '((a anything))))))
(define (raise-poly-var p var)
  (define (the-term-of poly) (compose first-term term-list))
  (define (make-singleton-term-list new-o new-c)
    (adjoin-term (make-term new-o new-c)
                 (the-empty-termlist)))
  (define (swap-nesting poly1)
    (let ((poly2 (coeff (first-term (term-list poly1)))))
      (let ((v1 (variable poly1))
            (v2 (variable poly2))
            (t1 (the-term-of poly1))
            (t2 (the-term-of poly2)))
        (make-polynomial v2
                         (make-singleton-term-list
                          (order t2)
                          (make-polynomial v1
                                           (make-singleton-term-list
                                            (order t1)
                                            (coeff t2))))))))
  ;; Three cases
  ;; var is not in the chain: Should return p
  ;; var is at the head of the chain: Should return p
  ;; var is buried more than one level deep in the chain: Should
  ;; return chain with p at head
  
  (let ((vp (variable p))
        (tp (the-term-of p)))
    (cond ((eq? vp var) p)                             ;case 2
          (else (swap-nesting (make-polynomial
                               (variable p)
                               (make-singleton-term-list
                                (order tp)
                                (raise-poly-var (coeff tp) var))))))))

(define (collect-var-polys poly-list var)
  (if (null? poly-list)
      nil
      (let ((first-poly (car poly-list))
            (rest-poly-list  (cdr poly-list)))
        (if (eq? (variable first-poly) var)
            (cons first-poly (collect-var-terms rest-poly-list var))
            (let ((term (first-term (term-list first-poly))))
              
              )))))

(define p1 (make-polynomial 'x '((3 1) (0 -1))))
(define p2 (make-polynomial 'x '((1 1) (0 -1))))
(define p3 (make-polynomial 'x '((2 1) (0 -9))))
(define p4 (make-polynomial 'x '((1 1) (0 -3))))
(define q1 (make-polynomial 'y (list (list 3 p2) (list 1 p1) (list 0 4))))
(define q2 (make-polynomial 'y (list (list 3 p4) (list 2 p3) (list 1 4))))
(define r1 (make-polynomial 'z (list (list 4 q2) (list 3 q1)
                                     (list 1 p3) (list 0 p4))))

;;--------------
;;EXERCISE 2.93:
;;--------------


;;; Modify the rational-arithmetic package to use generic operations,
;;; but change make-rat so that it does not attempt to reduce fractions
;;; to lowest terms. Test your system by calling make-rational on two
;;; polynomials to produce a rational function:

(define p1 (make-polynomial 'x '((2 1) (0 1))))
(define p2 (make-polynomial 'x '((3 1) (0 1))))
(define rf (make-rational p2 p1))

;;; Now add rf to itself, using add. You will observe that this
;;; addition procedure does not reduce fractions to lowest terms.

;;; Here is the modified rational-arithmetic package:
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    ;; (let ((g (greatest-common-divisor n d)))
    ;;   (cons (div n g) (div d g)))
    (cons n d))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  (define (equ? r1 r2) (equ? (mul (numer r1) (denom r2))
                          (mul (denom r1) (numer r2))))
  
  (define (=zero? r) (=zero? (numer r)))
    ;; interface to rest
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       equ?)
  (put '=zero? '(rational)
       =zero?)
  ;; raise procedure from ex. 2.83
  (put 'raise '(rational)
       (lambda (r) (make-real (/ (numer r) (denom r)))))
  ;; project procedure from ex. 2.85
  (put 'project '(rational)
       (lambda (r) (make-integer (round (/ (numer r) (denom r))))))
  
  ;; ;; generic functions from ex 2.86
  ;; (put 'sin-generic '(rational)
  ;;      (lambda (r) (make-real (sin (/ (numer r) (denom r))))))
  ;; (put 'cos-generic '(rational)
  ;;      (lambda (r) (make-real (cos (/ (numer r) (denom r))))))
  ;; (put 'square-generic '(rational)
  ;;      (lambda (r) (make-real (/  (* (numer r) (numer r))
  ;;                            (* (denom r) (denom r))))))
  ;; (put 'sqrt-generic '(rational)
  ;;      (lambda (r) (make-real (sqrt (/ (numer r) (denom r))))))
  ;; (put 'atan-generic '(rational rational)
  ;;      (lambda (x y) (make-real (atan (/ (numer x) (denom x))
  ;;                                (/ (numer x) (denom y))))))
  
  ;; negate from ex 2.88
  (put 'negate '(rational)
       (lambda (r) (tag (make-rat (sub (numer r)) (denom r)))))
  'done)

(install-rational-package)

;;-------------------------------------------------------
;;EXERCISE 2.94
;;-------------------------------------------------------

;;; Using div-terms, implement the procedure remainder-terms and use
;;; this to define gcd-terms as above. Now write a procedure gcd-poly
;;; that computes the polynomial GCD of two polys. (The procedure
;;; should signal an error if the two polys are not in the same
;;; variable.) Install in the system a generic operation
;;; greatest-common-divisor that reduces to gcd-poly for polynomials
;;; and to ordinary gcd for ordinary numbers. As a test, try

(define p1 (make-polynomial
            'x '((4 1) (3 -1) (2 -2) (1 2))))
(define p2 (make-polynomial 'x '((3 1) (1 -1))))

(greatest-common-divisor p1 p2)

;;; and check your result by hand.

;;; In the polynomial package:
(define (remainder-terms L1 L2)
  (cadr (div-terms L1 L2)))

(define (gcd-poly p1 p2)
  (define (gcd-terms t1 t2)
    (if (empty-termlist? t2)
        t1
        (gcd-terms t2 (remainder-terms t1 t2))))

  (if (same-variable? (variable p1)
                      (variable p2))
      (make-poly (variable p1)
                 (gcd-terms (term-list p1) (term-list p2)))
      (error "Polynomials are not of same variable (GCD-POLY)")))

(put 'greatest-common-divisor '(polynomial polynomial)
     (lambda (p1 p2) (tag (gcd-poly p1 p2))))

;;;
(greatest-common-divisor p1 p2)  ;; (polynomial x (2 -1) (1 1))

;;--------------
;;EXERCISE 2.95:
;;--------------

;;; Define P1 , P2 , and P3 to be the polynomials

;;; P1 : x 2 - 2x + 1,
;;; P2 : 11x 2 + 7,
;;; P3 : 13x + 5.

;;; Now define Q 1 to be the product of P1 and P2 and Q 2 to be the
;;; product of P1 and P3 , and use greatest-commondivisor (Exercise
;;; 2.94) to compute the GCD of Q 1 and Q 2 . Note that the answer is
;;; not the same as P1 . This example introduces noninteger operations
;;; into the computation, causing difficulties with the GCD
;;; algorithm. To understand what is happening, try tracing
;;; gcd-terms while computing the GCD or try performing the division
;;; by hand.

(define p1 (make-polynomial 'x '((2 1) (1 -2) (0 1))))
(define p2 (make-polynomial 'x '((2 11) (0 7))))
(define p3 (make-polynomial 'x '((1 13) (0 5))))

(define q1 (mul p1 p2))
(define q2 (mul p1 p3))

;;---------------
;;EXERCISE 2.96(a)
;;---------------

;;; Implement the procedure pseudoremainder-terms, which is just like
;;; remainder-terms except that it multiplies the dividend by the
;;; integerizing factor described above before calling div-terms.
;;; Modify gcd-terms to use pseudoremainder-terms, and verify that
;;; greatest- common-divisor now produces an answer with integer
;;; coefficients on the example in Exercise 2.95.

;;; In the polynomial package:
(define (pseudoremainder-terms L1 L2)
  
  (define (scale-terms L scalar)
    (if (empty-termlist? L)
        L
        (adjoin-term (make-term (order (first-term L))
                                (mul scalar (coeff first-term L)))
                     (scale-terms (rest-terms L) scalar))))
  
  (let ((c (coeff (first-term L2)))
        (o1 (order (first-term L1)))
        (o2 (order (first-term L2))))
    
    (cadr (div-terms (scale-terms L1
                                  (expt c (+ 1 o1 (- o2))))
                     L2))))

(define (gcd-poly p1 p2)
  (define (gcd-terms t1 t2)
    (if (empty-termlist? t2)
        t1
        (gcd-terms t2 (pseudoremainder-terms t1 t2))))

  (if (same-variable? (variable p1)
                      (variable p2))
      (make-poly (variable p1)
                 (gcd-terms (term-list p1) (term-list p2)))
      (error "Polynomials are not of same variable (GCD-POLY)")))

(put 'greatest-common-divisor '(polynomial polynomial)
     (lambda (p1 p2) (tag (gcd-poly p1 p2))))

;;---------------
;;EXERCISE 2.96(b)
;;---------------

;; The GCD now has integer coefficients, but they are larger than
;; those of P1 . Modify gcd-terms so that it removes common factors
;; from the coefficients of the answer by dividing all the
;; coefficients by their (integer) greatest common divisor.

;;; We need a way to get the integer gcd of more than two numbers using gcd.

;;; (gcd-many a b c d)
;;; (gcd a (gcd-many b c d))
;;; (gcd a (gcd b (gcd c d)))

(define (gcd-many . args)
  (cond
   ((null? args) (error "Too few arguments"))
   ((null? (cdr args)) (car args))
   (else (greatest-common-divisor (car args)
                                  (apply gcd-many (cdr args))))))

;;; The rest is straightforward:
;;; In the polynomial package:
(define (gcd-poly p1 p2)
    (define (gcd-terms t1 t2)
      (if (empty-termlist? t2)
          t1
          (gcd-terms t2 (pseudoremainder-terms t1 t2))))

    (define (map-terms proc terms)
      (if (null? terms)
          terms
          (cons (proc (first-term terms))
                (map-terms proc (rest-terms terms)))))
    
    (define (make-term-list orders coeffs)
      (if (null? orders)
          (the-empty-termlist)
          (adjoin-term (make-term (car orders) (car coeffs))
                       (make-term-list (cdr orders) (cdr coeffs)))))
    
    (define (gcd-terms-simplest terms)
      (let ((coeffs (map-terms coeff terms))
            (orders (map-terms order terms)))
        (let ((factor (apply gcd-many coeffs)))
          (make-term-list orders
                          (map (lambda (c) (/ c factor)) coeffs)))))
    
    (if (same-variable? (variable p1)
                        (variable p2))
        (make-poly (variable p1)
                   (gcd-terms-simplest
                    (gcd-terms (term-list p1) (term-list p2))))
        (error "Polynomials are not of same variable (GCD-POLY)")))

;;---------------
;;EXERCISE 2.97(a)
;;---------------

;;; Implement this algorithm as a procedure reduce-terms that takes
;;; two term lists n and d as arguments and returns a list nn, dd,
;;; which are n and d reduced to lowest terms via the algorithm given
;;; above. Also write a procedure reduce-poly, analogous to add-poly,
;;; that checks to see if the two polys have the same variable. If so,
;;; reduce-poly strips off the variable and passes the problem to
;;; reduce-terms, then reattaches the variable to the two term lists
;;; supplied by reduce-terms.

(define (reduce-terms L1 L2)
  (let ((G (gcd-terms L1 L2)))
    (list (factorize-coefficients (car (div-terms L1 G)))
          (factorize-coefficients (car (div-terms L2 G))))))

(define (reduce-poly p1 p2)
  (if (same-variable? (variable p1)
                      (variable p2))
      (let ((reduced-terms (reduce-terms (term-list p1)
                                         (term-list p2))))
        (list (make-polynomial (variable p1) (car  reduced-terms))
              (make-polynomial (variable p1) (cadr reduced-terms))))
      (error "Polynomials are not of same variable (REDUCE-POLY)")))

;;----------------
;;EXERCISE 2.97(b)
;;----------------

;;; Define a procedure analogous to reduce-terms that does what the
;;; original make-rat did for integers:

(define (reduce-integers n d)
  (let ((g (gcd n d)))
    (list (/ n g) (/ d g))))

;;; and define reduce as a generic operation that calls apply-generic
;;; to dispatch to either reduce-poly (for polynomial arguments) or
;;; reduce-integers (for scheme number arguments). You can now easily
;;; make the rational arithmetic package reduce fractions to lowest
;;; terms by having make-rat call reduce before combining the given
;;; numerator and denominator to form a rational number. The system
;;; now handles rational expressions in either integers or
;;; polynomials. To test your program, try the example at the
;;; beginning of this extended exercise.

;;; Install reduce as a generic operation
(define (reduce x y) (apply-generic 'reduce x y))

;;; The rational package is modified from Ex. 2.94 with
;;; reduce-integers and changes to make-rat.

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((reduced (reduce n d)))
      (cons (car reduced) (cadr reduced))))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  (define (equ? r1 r2) (equ? (mul (numer r1) (denom r2))
                          (mul (denom r1) (numer r2))))
  
  (define (=zero? r) (=zero? (numer r)))
    ;; interface to rest
  (define (tag x) (attach-tag 'rational x))
  
  ;; Ex. 2.97b
  (define (reduce-integers n d)
    (let ((g (gcd n d)))
      (list (/ n g) (/ d g))))
  
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  
  (put 'equ? '(rational rational)
       equ?)
  (put '=zero? '(rational)
       =zero?)
  ;; raise procedure from ex. 2.83
  (put 'raise '(rational)
       (lambda (r) (make-real (/ (numer r) (denom r)))))
  ;; project procedure from ex. 2.85
  (put 'project '(rational)
       (lambda (r) (make-integer (round (/ (numer r) (denom r))))))
  
  ;; negate from ex 2.88
  (put 'negate '(rational)
       (lambda (r) (tag (make-rat (sub (numer r)) (denom r)))))
  
  ;; ex. 2.97b
  (put 'reduce '(scheme-number scheme-number) reduce-integers)
  
  'done)

(install-rational-package)

;;; Results:
(define p1 (make-polynomial 'x '((2 1) (1 -2) (0 1))))
(define p2 (make-polynomial 'x '((2 11) (0 7))))
(define p3 (make-polynomial 'x '((1 13) (0 5))))
(define p4 (make-polynomial 'x '((2 1) (0 -1))))

(define q1 (mul p1 p2))
(polynomial x (4 11) (3 -22) (2 18) (1 -14) (0 7))

(define q2 (mul p1 p3))
(polynomial x (3 13) (2 -21) (1 3) (0 5))

(define rf1 (make-rational p1 p2))
(rational (polynomial x (2 1) (1 -2) (0 1)) polynomial x (2 11) (0 7))

(define rf2 (make-rational p3 p4))
(rational (polynomial x (1 13) (0 5)) polynomial x (2 -1) (0 1))

(make-rational q1 q2)
(rational (polynomial x (2 11) (0 7)) polynomial x (1 13) (0 5))

(add rf1 rf2)
(rational
 (polynomial x (4 -1) (3 145) (2 55) (1 89) (0 36))
 .
 (polynomial x (4 -11) (2 4) (0 7)))

;;; Aaaaand that's a wrap. Onto chapter 3. At last!

