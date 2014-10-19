(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

;----------------------------------------------------------------
(define global-array '())

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))
;----------------------------------------------------------------

;----------------------------------------------------------------
(define coercion-array '())

(define (put-coercion type1 type2 proc)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k proc)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! coercion-array (put-helper (list type1 type2) coercion-array)))

(define (get-coercion type1 type2)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list type1 type2) coercion-array))
;----------------------------------------------------------------

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

;;; TODO

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
      (let ((raise (get 'raise (type-tag arg1))))
        (if raise
            (raise-up-from (raise arg1) arg2)
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
(put 'project 'real
     (lambda (r) (make-rational ;WHAT HERE?
             )))

;;; in the rational number package
(put 'project 'rational
     (lambda (r) (round (/ (numer r) (denom r)))))

;;; Install project
(define (project num) (apply-generic 'project num))

(define (drop num)
  (if (equal? (type-tag num) 'scheme-number)
      num
      (let ((pnum (project num)))
        (if (equ? num (raise pnum))
            (drop pnum)
            pnum))))

;;; Simplified apply-generic
(define (apply-generic op . args)
  
  (define (raise-up-from arg1 arg2)
  ;; Raise arg1 into an entity of type arg2 and return it.
  (if (equal? (type-tag arg1) (type-tag arg2))
      arg1
      (let ((raise (get 'raise (type-tag arg1))))
        (if raise
            (raise-up-from (raise arg1) arg2)
            #f))))
  
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc                          ;;ONLY CHANGE
          (let ((res (apply proc (map contents args))))
            (if (equal? proc 'project) res (drop (res)))) 
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


