;;; Helper functions
(load "sicp-2.4-helper.scm")

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
  (put '=zero? 'scheme-number =zero?))

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
  (put '=zero? 'rational-number =zero?))

(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;;--------------
;;EXERCISE 2.81:
;;--------------

;;; Louis Reasoner has noticed that apply-generic may try to coerce
;;; the arguments to each other's type even if they already have the
;;; same type. Therefore, he reasons, we need to put procedures in the
;;; coercion table to coerce arguments of each type to their own type.
;;; For example, in addition to the scheme-number->complex coercion
;;; shown above, he would do:

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

;;; With Louis's coercion procedures installed, what happens if
;;; apply-generic is called with two arguments of type scheme-number
;;; or two arguments of type complex for an operation that is not
;;; found in the table for those types? For example, assume that we've
;;; defined a generic exponentiation operation:

(define (exp x y) (apply-generic 'exp x y))

;;; and have put a procedure for exponentiation in the Scheme-number
;;; package but not in any other package:

;; following added to Scheme-number package
(put 'exp '(scheme-number scheme-number)
     (lambda (x y) (tag (expt x y)))) ; using primitive expt

;;; What happens if we call exp with two complex numbers as arguments?

;;; Ans: apply-generic gets stuck in an infinite recursion call. It
;;; coerces complex->complex and calls itself.

;;; Is Louis correct that something had to be done about coercion with
;;; arguments of the same type, or does apply-generic work correctly
;;; as is?

;;; Ans: Apply-generic will work fine as it is.

;;; Modify apply-generic so that it doesn't try coercion if the two
;;; arguments have the same type.

;;; We need to add a clause to apply-generic:

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                ;; Check if the types are the same
                (if (not (eq? type1 type2))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else
                             (error "No method for these types"
                                    (list op type-tags)))))
                    (error "No method for these types"
                           (list op type-tags))))
              (error "No method for these types"
                     (list op type-tags)))))))

;;--------------
;;EXERCISE 2.82:
;;--------------

;;; Show how to generalize apply-generic to handle coercion in the
;;; general case of multiple arguments. One strategy is to attempt to
;;; coerce all the arguments to the type of the first argument, then
;;; to the type of the second argument, and so on. Give an example of
;;; a situation where this strategy (and likewise the two-argument
;;; version given above) is not sufficiently general. (Hint: Consider
;;; the case where there are some suitable mixed-type operations
;;; present in the table that will not be tried.)

;;; This strategy is not sufficiently general for, say, this
;;; expression: (* a z1 z2), where a is real, z1/2 are complex. If a
;;; is coerced to be complex, we might miss the 'scale operation in
;;; the table (from the complex number package) that scales a complex
;;; number z by a scalar a.

;;; In the complex number package:
;; (define (scale a z)
;;   (make-from-real-imag (* a (real-part z))
;;                        (* a (imag-part z))))

;; (put 'scale '(real complex) scale)

;;; What we need is a full search over the "cross-product" of the set
;;; '(a1 a2 a3 a4 ...), where ai are the arguments, and the set of
;;; coercion procedures '(t1->t2 t2->t3 t3->t1...). (Note that some of
;;; these procedures may not exist in the coercion table).

;;; Generate all permutations of '(these tag types).
;;; Gives '((these these these)
;;;         (these these tag)
;;;         (these tag these)...)
(define (permutations args positions)
  (define (adjoin-term term)
    (map (lambda (perm-lst) (cons term perm-lst))
         (permutations args (- positions 1))))
  (if (zero? positions)
      '(())
      (flatmap adjoin-term args)))

(define (in-coercion-table? my-type-tags perm)
  (if (null? perm)
      #t
      (and (or
            ;; same type, no coercion needed:
            (eq? (car my-type-tags) (car perm))
            ;; coercion possible:
            (get-coercion (car my-type-tags) (car perm))) 
           (in-coercion-table? (cdr my-type-tags) (cdr perm)))))

;;; Keep only the type combinations that can be enforced through
;;; coercion and operated upon by a prcedure from the get/put table.
(let ((coerced-args 
       (filter (lambda (perm) (and (get 'add perm)
                              (in-coercion-table? type-tags perm)))
               (permutations type-tags (length type-tags)))))

;;; Every element of the list above is a possible coercion of the
;;; argument types that can be operated upon. Just pick the first one
;;; and use apply-generic.
  (if (not (null? (car coerced-args)))
      (apply-generic op . (car coerced-args))
      (error "No method for these types" (list args type-tags))))


;;; apply-generic modified with these changes:

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    
    ;; Helper procedures
    (define (permutations args positions)
      (define (adjoin-term term)
        (map (lambda (perm-lst) (cons term perm-lst))
             (permutations args (- positions 1))))
      (if (zero? positions)
          '(())
          (flatmap adjoin-term args)))  
    
    (define (in-coercion-table? my-type-tags perm)
      (if (null? perm)
          #t
          (and (or
                ;; same type, no coercion needed:
                (eq? (car my-type-tags) (car perm))
                ;; coercion possible:
                (get-coercion (car my-type-tags) (car perm))) 
               (in-coercion-table? (cdr my-type-tags) (cdr perm)))))
    
    (define (coerce-args new-type-tags)
      (map (lambda (f arg) (f arg))
           (map (lambda (old-type new-type)
                  (or  (get-coercion old-type new-type) identity))
                type-tags new-type-tags)
           args))
    
    ;; Applying op
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((coerced-types 
                 (filter (lambda (perm) (and (get op perm)
                                        (in-coercion-table? type-tags perm)))
                         (permutations type-tags (length type-tags)))))
            (if (not (null? (car coerced-types)))
                (apply apply-generic (cons op (coerce-args (car coerced-types))))
                (error "No method for these types" (list args type-tags))))))))

;;--------------
;;EXERCISE 2.83:
;;--------------

;;; Suppose you are designing a generic arithmetic system for dealing
;;; with the tower of types shown in Figure 2-25: integer, rational,
;;; real, complex. For each type (except complex), design a procedure
;;; that raises objects of that type one level in the tower. Show how
;;; to install a generic raise operation that will work for each type
;;; (except complex).

;;; in the scheme-number package
(put 'raise 'integer 
     (lambda (raise i) (make-rational i 1)))

;;; in the rational package
(put 'raise 'rational
     (lambda (raise r) (make-real (/ (numer r) (denom r)))))

;;; in the real package
(put 'raise 'real
     (lambda (raise r) (make-from-real-imag r 0)))

;;; This raise operation will work for each type.
(define (raise num) (apply-generic 'raise num))

;;-------------
;;EXERCISE 2.84
;;-------------

;;; Using the raise operation of Exercise 2-83, modify the
;;; apply-generic procedure so that it coerces its arguments to have
;;; the same type by the method of successive raising, as discussed in
;;; this section. You will need to devise a way to test which of two
;;; types is higher in the tower. Do this in a manner that is
;;; "compatible" with the rest of the system and will not lead to
;;; problems in adding new levels to the tower.

;;; Test which of two types is higher in the tower:

;;; The easy way to do this would be to prepare a data structure that
;;; holds the tower of types as a list and check to see which type
;;; comes first. But this list will need to be updated when adding new
;;; levels, so let's try to answer this question without it.

(define (apply-generic op . args)
  
  (define (raise-up-from arg1 arg2)
  ;; Raise arg1 into an entity of type arg2 and return it.
  (if (equal? (type-tag arg1) (type-tag arg2))
      arg1
      (let ((raise (get 'raise (list (type-tag arg1)))))
        (if raise
            (raise-up-from (apply-generic 'raise arg1) arg2)
            ;; (note: (raise arg1) does not work in the above
            ;; expression. This is because arg1 has a type-tag to be
            ;; stripped, the raise procedure does not strip it.
            #f))))
  
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                
                (let ((a1->type2 (raise-up-from a1 a2))
                      (a2->type1 (raise-up-from a2 a1)))
                  (cond (a1->type2
                         (apply-generic op a1->type2 a2))
                        (a2->type1
                         (apply-generic op a1 a2->type1))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;;--------------
;;EXERCISE 2.85:
;;--------------

;;; This section mentioned a method for "simplifying" a data object by
;;; lowering it in the tower of types as far as possible. Design a
;;; procedure drop that accomplishes this for the tower described in
;;; Exercise 2-83. The key is to decide, in some general way, whether
;;; an object can be lowered. For example, the complex number 1.5+0i
;;; can be lowered as far as real, the complex number 1+0i can be
;;; lowered as far as integer, and the complex number 2+3i cannot be
;;; lowered at all. Here is a plan for determining whether an object
;;; can be lowered: Begin by defining a generic operation project that
;;; "pushes" an object down in the tower. For example, projecting a
;;; complex number would involve throwing away the imaginary part.
;;; Then a number can be dropped if, when we project it and raise the
;;; result back to the type we started with, we end up with something
;;; equal to what we started with. Show how to implement this idea in
;;; detail, by writing a drop procedure that drops an object as far as
;;; possible. You will need to design the various projection
;;; operations and install project as a generic operation in the
;;; system. You will also need to make use of a generic equality
;;; predicate, such as described in Exercise 2-79. Finally, use drop
;;; to rewrite apply-generic from Exercise 2-84 so that it
;;; "simplifies" its answers.

;;; The logic for project is different based on the type of number
;;; being pushed down the tower. So we have one for each package:

;;; in the complex number package
(put 'project 'complex
     (lambda (c) (make-real (real-part c))))

;;; in the real number package
(put 'project '(scheme-number)
       (lambda (n)
         (cond ((integer? n)
                n)
               (else
                (let ((r (rationalize (inexact->exact n) 1/1000)))
                  (make-rational (numerator r)
                                 (denominator r)))))))


;;; in the rational number package
(put 'project 'rational
     (lambda (r) (round (/ (numer r) (denom r)))))

;;; Install project
(define (project num) (apply-generic 'project num))

(define (drop num)
  (if (or (number? num)
          (equal? (type-tag num) 'integer))
      num
      (let ((pnum (project num)))
        (if (equ? num (raise pnum))
            (drop pnum)
            num))))

;;; Simplified apply-generic
(define (apply-generic op . args)
  
  (define (raise-up-from arg1 arg2)
  ;; Raise arg1 into an entity of type arg2 and return it.
  (if (equal? (type-tag arg1) (type-tag arg2))
      arg1
      (let ((raise (get 'raise (list (type-tag arg1)))))
        (if raise
            (raise-up-from (apply-generic 'raise arg1) arg2)
            #f))))
  
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc                          ;;ONLY CHANGE
          (let ((res (apply proc (map contents args))))
            (if (or  (equal? op 'project)
                     (equal? op 'raise)
                     (equal? op 'equ?))
                res
                (drop res))) 
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                
                (let ((a1->type2 (raise-up-from a1 a2))
                      (a2->type1 (raise-up-from a2 a1)))
                  (cond (a1->type2
                         (apply-generic op a1->type2 a2))
                        (a2->type1
                         (apply-generic op a1 a2->type1))
                        (else
                         (error "No coercion method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))


;;--------------
;;EXERCISE 2.86:
;;--------------

;;; Suppose we want to handle complex numbers whose real parts,
;;; imaginary parts, magnitudes, and angles can be either ordinary
;;; numbers, rational numbers, or other numbers we might wish to add
;;; to the system. Describe and implement the changes to the system
;;; needed to accommodate this. You will have to define operations
;;; such as sine and cosine that are generic over ordinary numbers and
;;; rational numbers.

;;; We define generic sine and cosine functions:
(define (sin-generic x) (apply-generic 'sin-generic x))
(define (cos-generic x) (apply-generic 'cos-generic x))
(define (square-generic x) (apply-generic 'square-generic x))
(define (sqrt-generic x) (apply-generic 'sqrt-generic x))
(define (atan-generic x y) (apply-generic 'atan-generic x y))

;;; In the scheme-number package, we add
(put 'sin-generic '(scheme-number) (make-real  (lambda (r) (sin r))))
(put 'cos-generic '(scheme-number) (make-real (lambda (r) (cos r))))
(put 'square-generic '(scheme-number) (make-real (lambda (r) (* r r))))
(put 'sqrt-generic '(scheme-number) (make-real (lambda (r) (sqrt r))))
(put 'atan-generic '(scheme-number scheme-number)
     (lambda (x y) (make-real (atan x y))))

;;; In the real package, we add
(put 'sin-generic '(real) sin-generic)
(put 'cos-generic '(real) cos-generic)
(put 'square-generic '(real) square-generic)
(put 'sqrt-generic '(real) sqrt-generic)
(put 'atan-generic '(real real) atan-generic)

;;; Ditto with the integer package,
(put 'sin-generic '(integer) sin-generic)
(put 'cos-generic '(integer) cos-generic)
(put 'square-generic '(integer) square-generic)
(put 'sqrt-generic '(integer) sqrt-generic)
(put 'atan-generic '(integer integer) atan-generic)

;;; In the rational number package, we add
(put 'sin-generic '(rational) (lambda (r) (make-real (sin (/ (numer r) (denom r))))))
(put 'cos-generic '(rational) (lambda (r) (make-real (cos (/ (numer r) (denom r))))))
(put 'square-generic '(rational) (lambda (r) (make-real
                                         (/  (* (numer r) (numer r))
                                             (* (denom r) (denom r))))))
(put 'sqrt-generic '(rational) (lambda (r) (make-real (sqrt (/ (numer r)
                                                          (denom r))))))
(put 'atan-generic '(rational rational)
     (lambda (x y) (make-real (atan (/ (numer x) (denom x))
                               (/ (numer x) (denom y))))))

;;; And we change install-polar-package and
;;; install-rectangular-package as follows:
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cos-generic (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sin-generic (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt-generic (add (square-generic x) (square-generic y)))
          (atan-generic y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt-generic (add (square-generic (real-part z))
                       (square-generic (imag-part z)))))
  (define (angle z)
    (atan-generic (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (mul r (cos-generic a)) (mul r (sin-generic a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;;; The complex package needs generic add/sub etc in place of +/-

;;; Complex number package
;----------------------------------------------------------------
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))

  (define (equ? z1 z2) (and (equ? (real-part z1) (real-part z2))
                            (equ? (imag-part z1) (imag-part z2))))
  
  (define (=zero? z) (= 0 (real-part z) (imag-part z)))
  
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  
  (put '=zero? '(complex)
       =zero?)
  (put 'equ? '(complex complex)
       equ?)
  
  ;; project procedure from ex. 2.85
  (put 'project '(complex)
     (lambda (c) (make-real (real-part c))))
  'done)

