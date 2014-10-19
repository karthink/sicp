;;; Helper functions
(load "sicp-2-helper.scm")

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;;--------------
;;EXERCISE 2.77:
;;--------------

;;; Louis Reasoner tries to evaluate the expression (magnitude z)
;;; where z is the object shown in Figure 2-24. To his surprise,
;;; instead of the answer 5 he gets an error message from
;;; apply-generic, saying there is no method for the operation
;;; magnitude on the types (complex). He shows this interaction to
;;; Alyssa P. Hacker, who says "The problem is that the complex-number
;;; selectors were never defined for complex numbers, just for polar
;;; and rectangular numbers. All you have to do to make this work is
;;; add the following to the complex package:"

(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

;;; Describe in detail why this works. As an example, trace through
;;; all the procedures called in evaluating the expression (magnitude
;;; z) where z is the object shown in Figure 2-24. In particular, how
;;; many times is apply-generic invoked? What procedure is dispatched
;;; to in each case?

;;; This works because z is tagged as 'complex, and this adds the
;;; magnitude function for that tag in the table. 

;;; (define (magnitude z) (apply-generic 'magnitude z))

;;; When calling (magnitude z), the following happens:

;;; (apply-generic 'magnitude z)
;;; strips the 'complex tag and calls (magnitude z)
;;; calls (apply-generic 'magnitude z)
;;; strips the 'rectangular or 'polar tag and calls the correct
;;; magnitude function.

;;; The first dispatch is to the magnitude function of 'complex, the
;;; second to the magnitude function of the package 'rectangular.

;;; So apply-generic is called twice.

;;--------------
;;EXERCISE 2.78:
;;--------------

;;; The internal procedures in the scheme-number package are
;;; essentially nothing more than calls to the primitive procedures +,
;;; -, etc. It was not possible to use the primitives of the language
;;; directly because our type-tag system requires that each data
;;; object have a type attached to it. In fact, however, all Lisp
;;; implementations do have a type system, which they use internally.
;;; Primitive predicates such as symbol? and number? determine whether
;;; data objects have particular types. Modify the definitions of
;;; type-tag, contents, and attach-tag from section 2-4-2 so that our
;;; generic system takes advantage of Scheme's internal type system.
;;; That is to say, the system should work as before except that
;;; ordinary numbers should be represented simply as Scheme numbers
;;; rather than as pairs whose car is the symbol scheme-number.

;;; Original:

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum -- CONTENTS" datum))))

;;--------------------
;;EXERCISE 2.79 & 2.80:
;;--------------------

;;; Define a generic equality predicate equ? that tests the equality
;;; of two numbers, and install it in the generic arithmetic package.
;;; This operation should work for ordinary numbers, rational numbers,
;;; and complex numbers.

;;; Define a generic predicate =zero? that tests if its argument is
;;; zero, and install it in the generic arithmetic package. This
;;; operation should work for ordinary numbers, rational numbers, and
;;; complex numbers.

;;;We add three different procedures:

;;; This one to the generic number package
(define install-scheme-number-package
  ;;;...
  (define equ? =)
  (put 'equ? '(scheme-number scheme-number) equ?)

  (define (=zero? n) (= n 0))
  (put '=zero? 'scheme-number =zero?)
  )

;;; This one to the complex number package
(define install-complex-package
  ;;;...
  (define (equ? z1 z2) (and (= (real-part z1) (real-part z2))
                            (= (imag-part z1) (imag-part z2))))
  (put 'equ? '(complex-number complex-number) equ?)

  (define (=zero? z) (= 0 (real-part z) (imag-part z)))
  (put '=zero? 'complex-number =zero?))

;;; This one to the rational number package
(define install-rational-package
  ;;;...
  (define (equ? r1 r2) (= (* (numer r1) (denom r2))
                          (* (denom r1) (numer r2))))
  (put 'equ? '(rational-number rational-number) equ?)

  (define (=zero? r) (= 0 (numer r)))
  (put '=zero? 'rational-number =zero?)))

(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
