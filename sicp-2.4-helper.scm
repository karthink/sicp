;;-------------------
;;PRIMITIVE FUNCTIONS
;;-------------------
;----------------------------------------------------------------
(define (compose f g) (lambda (x) ( f (g x))))
(define (inc x)  (+ x 1))
(define (square x) (* x x))
(define nil '())
(define identity (lambda (entity) entity))
(define (gcd a b)
  (cond ((> (abs b) (abs a)) (gcd b a))
        ((= b 0) a)
        (else (gcd b (remainder a b)))))
;----------------------------------------------------------------

;;-------------------
;;SEQUENCE OPERATIONS
;;-------------------
;----------------------------------------------------------------
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence)) (cons (car sequence)
                                          (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (deep-member? predicate k lst)
  (cond ((null? lst) #f)
        ((pair? (car lst)) (or (deep-member? predicate k (car lst))
                               (deep-member? predicate k (cdr lst))))
        ((predicate k (car lst)) #t)
        (else (deep-member? predicate k (cdr lst)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
;----------------------------------------------------------------

;;------
;;TABLES
;;------
;;; Table implementation for put and get
;----------------------------------------------------------------
(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

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

;;; Table implementation for put-coercion and get-coercion
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

;;--------------------------------------
;;TAGGING SYSTEM FOR DATA REPRESENTATION
;;--------------------------------------
;----------------------------------------------------------------
;; (define (attach-tag type-tag contents)
;;   (cons type-tag contents))

;; (define (type-tag datum)
;;   (if (pair? datum)
;;       (car datum)
;;       (error "Bad tagged datum -- TYPE-TAG" datum)))

;; (define (contents datum)
;;   (if (pair? datum)
;;       (cdr datum)
;;       (error "Bad tagged datum -- CONTENTS" datum)))
;----------------------------------------------------------------
;;; From Ex. 2.78 -- scheme-numbers work automatically
;;; Required for polynomial (coefficient) operations
(define (attach-tag type-tag contents)
  (if (and (eq? type-tag 'scheme-number)
           (number? contents))
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

;;-----------------------------
;;ARITHMETIC PACKAGES FROM SICP
;;-----------------------------

;;; Includes modifications from ex. 2.79 through 2.83
;;; =zero? and equ?
;----------------------------------------------------------------
;;; Complex number representations
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

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
  ;; From ex 2.88
  (put 'negate '(polar)
       (lambda (z) (tag (make-from-mag-ang
                    (magnitude z)
                    (+ (angle z) 3.141592653)))))
  'done)

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

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
  ;; From ex 2.88
  (put 'negate '(rectangular)
       (lambda (z) (tag  (make-from-real-imag
                     (- (real-part z))
                     (- (imag-part z))))))
  'done)

;; ;;; Complex number package
;; ;----------------------------------------------------------------
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  (define (equ? z1 z2) (and (= (real-part z1) (real-part z2))
                            (= (imag-part z1) (imag-part z2))))
  
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
  
  ;; From 2.88
  (put 'negate '(complex) (lambda (z) (tag (negate z))))
  ;; OR
  ;; (put 'negate '(complex)
  ;;      (lambda (z) (tag
  ;;              (sub-complex (make-from-real-imag 0 0) z))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;;; Rational number package
;----------------------------------------------------------------
;; (define (install-rational-package)
;;   ;; internal procedures
;;   (define (numer x) (car x))
;;   (define (denom x) (cdr x))
;;   (define (make-rat n d)
;;     (let ((g (gcd n d)))
;;       (cons (/ n g) (/ d g))))
;;   (define (add-rat x y)
;;     (make-rat (+ (* (numer x) (denom y))
;;                  (* (numer y) (denom x)))
;;               (* (denom x) (denom y))))
;;   (define (sub-rat x y)
;;     (make-rat (- (* (numer x) (denom y))
;;                  (* (numer y) (denom x)))
;;               (* (denom x) (denom y))))
;;   (define (mul-rat x y)
;;     (make-rat (* (numer x) (numer y))
;;               (* (denom x) (denom y))))
;;   (define (div-rat x y)
;;     (make-rat (* (numer x) (denom y))
;;               (* (denom x) (numer y))))
;;   (define (equ? r1 r2) (= (* (numer r1) (denom r2))
;;                           (* (denom r1) (numer r2))))
  
;;   (define (=zero? r) (= 0 (numer r)))
;;     ;; interface to rest of the system
;;   (define (tag x) (attach-tag 'rational x))
;;   (put 'add '(rational rational)
;;        (lambda (x y) (tag (add-rat x y))))
;;   (put 'sub '(rational rational)
;;        (lambda (x y) (tag (sub-rat x y))))
;;   (put 'mul '(rational rational)
;;        (lambda (x y) (tag (mul-rat x y))))
;;   (put 'div '(rational rational)
;;        (lambda (x y) (tag (div-rat x y))))

;;   (put 'make 'rational
;;        (lambda (n d) (tag (make-rat n d))))
;;   (put 'equ? '(rational rational)
;;        equ?)
;;   (put '=zero? '(rational)
;;        =zero?)
;;   ;; raise procedure from ex. 2.83
;;   (put 'raise '(rational)
;;        (lambda (r) (make-real (/ (numer r) (denom r)))))
;;   ;; project procedure from ex. 2.85
;;   (put 'project '(rational)
;;        (lambda (r) (make-integer (round (/ (numer r) (denom r))))))
  
;;   ;; generic functions from ex 2.86
;;   (put 'sin-generic '(rational)
;;        (lambda (r) (make-real (sin (/ (numer r) (denom r))))))
;;   (put 'cos-generic '(rational)
;;        (lambda (r) (make-real (cos (/ (numer r) (denom r))))))
;;   (put 'square-generic '(rational)
;;        (lambda (r) (make-real (/  (* (numer r) (numer r))
;;                              (* (denom r) (denom r))))))
;;   (put 'sqrt-generic '(rational)
;;        (lambda (r) (make-real (sqrt (/ (numer r) (denom r))))))
;;   (put 'atan-generic '(rational rational)
;;        (lambda (x y) (make-real (atan (/ (numer x) (denom x))
;;                                  (/ (numer x) (denom y))))))
  
;;   ;; negate from ex 2.88
;;   (put 'negate '(rational)
;;        (lambda (r) (tag (make-rat (- (numer r)) (denom r)))))
;;   'done)

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

(define (make-rational n d)
  ((get 'make 'rational) n d))

;;; Scheme number package; is the basis for the integer and real
;;; packages
;----------------------------------------------------------------
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
  (put 'equ? '(scheme-number scheme-number) =)
  (put '=zero? '(scheme-number)
       (lambda (n) (= n 0)))
  ;; raise procedure from ex. 2.83
  (put 'raise '(scheme-number)
       (lambda (n) (make-complex-from-real-imag n 0)))
  ;; project procedure from ex. 2.85
  (put 'project '(scheme-number)
       (lambda (n)
         (let ((r (rationalize (inexact->exact n) 1/1000)))
           (make-rational (numerator r)
                          (denominator r)))))
  ;; From ex. 2.86
  (put 'sin-generic '(scheme-number) (lambda (r) (make-real (sin r))))
  (put 'cos-generic '(scheme-number) (lambda (r) (make-real (cos r))))
  (put 'square-generic '(scheme-number) (lambda (r) (make-real (* r r))))
  (put 'sqrt-generic '(scheme-number) (lambda (r) (make-real (sqrt r))))
  (put 'atan-generic '(scheme-number scheme-number)
       (lambda (x y) (make-real (atan x y))))

  ;; From ex. 288
  (put 'negate '(scheme-number) (lambda (r) (make-scheme-number (- r))))
  
  ;; From ex. 2.92, polynomial algebra support:
  (put 'expand '(scheme-number) (lambda (r) (list r)))
  
  ;; From ex. 2.94, generic gcd
  (put 'greatest-common-divisor '(scheme-number scheme-number)
       (lambda (r1 r2) (gcd r1 r2)))
  
  'done)

(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (define (make-real x)
    ((get 'make 'scheme-number) (+ x 0.0)))
  (put 'make 'real (lambda (x) (tag (make-real x))))
  (put 'add '(real real) (lambda (x y) (tag (add x y))))
  (put 'sub '(real real) (lambda (x y) (tag (sub x y))))
  (put 'mul '(real real) (lambda (x y) (tag (mul x y))))
  (put 'div '(real real) (lambda (x y) (tag (div x y))))
  (put 'equ? '(real real) (lambda (x y) (equ? x y)))
  (put '=zero? '(real) (lambda (n) (=zero? x y)))
  (put 'raise '(real) raise)
  (put 'project '(real) project)
  ;; From ex. 2.86
  (put 'sin-generic '(real) sin-generic)
  (put 'cos-generic '(real) cos-generic)
  (put 'square-generic '(real) square-generic)
  (put 'sqrt-generic '(real) sqrt-generic)
  (put 'atan-generic '(real real) atan-generic)
  ;; From ex 2.88
  (put 'negate '(real) (lambda (r) (tag (negate r))))
  'done)

(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
  (define (make-integer x)
    ((get 'make 'scheme-number) x))
  (put 'make 'integer (lambda (x) (tag (make-integer x))))
  (put 'add '(integer integer) (lambda (x y) (tag (add x y))))
  (put 'sub '(integer integer) (lambda (x y) (tag (sub x y))))
  (put 'mul '(integer integer) (lambda (x y) (tag (mul x y))))
  (put 'div '(integer integer) (lambda (x y) (tag (div x y))))
  (put 'equ? '(integer integer) (lambda (x y) (equ? x y)))
  (put '=zero? '(integer) (lambda (n) (=zero? n)))
  ;; This is ugly, but necessary:
  (put 'raise '(integer) (lambda (n) (make-rational (contents n) 1)))
  ;; From ex. 2.86
  (put 'sin-generic '(integer) sin-generic)
  (put 'cos-generic '(integer) cos-generic)
  (put 'square-generic '(integer) square-generic)
  (put 'sqrt-generic '(integer) sqrt-generic)
  (put 'atan-generic '(integer integer) atan-generic)
  ;; From ex. 2.88
  (put 'negate '(integer) (lambda (r) (tag (negate r))))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
(define (make-real n)
  ((get 'make 'real) n))
(define (make-integer n)
  ((get 'make 'integer) n))

;;; Generic operations
;----------------------------------------------------------------
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
;;; from ex. 2.79
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
;;; from ex. 2.83
(define (raise num) (apply-generic 'raise num))
;;; from ex. 2.85
(define (project num) (apply-generic 'project num))
;;; from ex. 2.86
(define (sin-generic x) (apply-generic 'sin-generic x))
(define (cos-generic x) (apply-generic 'cos-generic x))
(define (square-generic x) (apply-generic 'square-generic x))
(define (sqrt-generic x) (apply-generic 'sqrt-generic x))
(define (atan-generic x y) (apply-generic 'atan-generic x y))
;;; From ex 2.88
(define (negate x) (apply-generic 'negate x))

;;; From ex. 2.94
(define (greatest-common-divisor x y)
  (apply-generic 'greatest-common-divisor x y))

(define (gcd-many . args)
  (cond
   ((null? args) (error "Too few arguments"))
   ((null? (cdr args)) (car args))
   (else (greatest-common-divisor (car args)
                                  (apply gcd-many (cdr args))))))

;;; From ex. 2.97
(define (reduce x y) (apply-generic 'reduce x y))

;;; Populate coercion table
;----------------------------------------------------------------
(put-coercion 'real 'complex
              (lambda (r) (make-complex-from-real-imag
                      (contents (contents r)) 0)))

(put-coercion 'rational 'real
              (lambda (rat)
                (let ((r (contents rat)))
                  (make-real (/ (car r) (cdr r))))))

;;; apply-generic with coercion
;----------------------------------------------------------------
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          ;; No proc, try coercion
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

;;; Install all packages
(install-polar-package)
(install-rectangular-package)
(install-complex-package)
;; (install-rational-package)
(install-scheme-number-package)
(install-real-package)
(install-integer-package)









;; (define (install-real-package)
;;   (define (tag x)
;;     (attach-tag 'real x))
;;   (define (make-real x)
;;     ((get 'make 'scheme-number) x))
;;   (define (add-real x y)
;;     ((get 'add '(scheme-number scheme-number)) x y))
;;   (define (sub-real x y)
;;     ((get 'sub '(scheme-number scheme-number)) x y))
;;   (define (mul-real x y)
;;     ((get 'mul '(scheme-number scheme-number)) x y))
;;   (define (div-real x y)
;;     ((get 'div '(scheme-number scheme-number)) x y))
  
;;   (put 'make 'real (lambda (x) (tag (make-real x))))
;;   (put 'add 'real (lambda (x y) (tag (add-real x y))))
;;   (put 'sub 'real (lambda (x y) (tag (sub-real x y))))
;;   (put 'mul 'real (lambda (x y) (tag (mul-real x y))))
;;   (put 'div 'real (lambda (x y) (tag (div-real x y))))
;;   (put 'equ? '(real real)
;;        (lambda (x y)
;;          ((get 'equ? '(scheme-number scheme-number)) x y)))
;;   (put '=zero? '(real)
;;        (lambda (n)
;;          ((get '=zero? '(scheme-number)) n)))
;;   ;; raise procedure from ex. 2.83
;;   (put 'raise '(real)
;;        (lambda (r) (make-complex-from-real-imag r 0)))
;;   'done)

;; (define (install-integer-package)
;;   (define (tag x)
;;     (attach-tag 'integer x))
;;   (define (make-integer x)
;;     ((get 'make 'scheme-number) x))
;;   (define (add-integer x y)
;;     ((get 'add '(scheme-number scheme-number)) x y))
;;   (define (sub-integer x y)
;;     ((get 'sub '(scheme-number scheme-number)) x y))
;;   (define (mul-integer x y)
;;     ((get 'mul '(scheme-number scheme-number)) x y))
;;   (define (div-integer x y)
;;     ((get 'div '(scheme-number scheme-number)) x y))
  
;;   (put 'make 'integer (lambda (x) (tag (make-integer x))))
;;   (put 'add 'integer (lambda (x y) (tag (add-integer x y))))
;;   (put 'sub 'integer (lambda (x y) (tag (sub-integer x y))))
;;   (put 'mul 'integer (lambda (x y) (tag (mul-integer x y))))
;;   (put 'div 'integer (lambda (x y) (tag (div-integer x y))))
;;   (put 'equ? '(integer integer)
;;        (lambda (x y)
;;          ((get 'equ? '(scheme-number scheme-number)) x y)))
;;   (put '=zero? '(integer)
;;        (lambda (n)
;;          ((get '=zero? '(scheme-number)) n)))
;;   ;; raise procedure from ex. 2.83
;;   (put 'raise '(integer) (lambda (r) (make-rational r 1)))
;;   'done)
