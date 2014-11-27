
;;; Polynomial package
(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  
  ;; <procedures same-variable? and variable? from section 2.3.2>
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  ;; representation of terms and term lists
  ;; <procedures adjoin-term . . . coeff from text below >
  
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))

  (define (make-term order coeff)
    (list order coeff))
  
  (define (the-empty-termlist) '())
  (define (empty-termlist? term-list) (null? term-list))
  
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: ADD-POLY" (list p1 p2))))
  
  ;;<procedures used by add-poly>
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else (let ((t1 (first-term L1))
                      (t2 (first-term L2)))
                  (cond ((> (order t1) (order t2))
                         (adjoin-term t1 (add-terms (rest-terms L1)
                                                    L2)))
                        ((> (order t2) (order t1))
                         (adjoin-term t2 (add-terms L1
                                                    (rest-terms L2))))
                        (else (adjoin-term
                               (make-term (order t1)
                                          (add (coeff t1) (coeff t2)))
  ;; Note: we used generic add to add coefficients. This lets us dispatch.
                               (add-terms (rest-terms L1)
                                          (rest-terms L2)))))))))
    (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))
  ;; <procedures used by mul-poly>
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  ;; Ex 2.88
  (define (negate-poly p)
    (make-poly (variable p) (negate-terms (term-list p))))

  (define (negate-terms terms)
    (if (empty-termlist? terms)
        (the-empty-termlist)
        (let ((t1 (first-term terms)))
          (adjoin-term (make-term (order t1) (negate (coeff t1)))
                       (negate-terms (rest-terms terms))))))
  
  
  (define (sub-terms L1 L2) (add-terms L1 (negate-terms L2)))
  
  ;; Ex. 2.91
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((quotient-and-remainder (div-terms (term-list p1) (term-list p2))))
          (list (make-poly (variable p1)
                           (car quotient-and-remainder))
                (make-poly (variable p1)
                           (cadr quotient-and-remainder))))
        (error "Polys not in same var: DIV-POLY" (list p1 p2))))

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
  
  ;; Ex 2.92: TESTING
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

  ;; Ex 2.93: Support for equ?
  (define (equ-poly? p1 p2)
    (define (same-term-list? t1 t2)
      (if (and (empty-termlist? t1)
               (empty-termlist? t2))
          #t
          (let ((f1 (first-term t1)) (f2 (first-term t2)))
            (and (equ? (order f1) (order f2))
                 (equ? (coeff f1) (coeff f2))
                 (same-term-list? (rest-terms t1)
                                  (rest-terms t2))))))
    (and (same-variable? (variable p1)
                         (variable p2))
         (same-term-list? (term-list p1)
                          (term-list p2))))
  
  ;; Ex 2.94: Remainder-terms & gcd-poly
  ;; (define (remainder-terms L1 L2)
  ;;   (cadr (div-terms L1 L2)))
  
  ;; (define (gcd-poly p1 p2)
  ;;   (define (gcd-terms t1 t2)
  ;;     (if (empty-termlist? t2)
  ;;         t1
  ;;         (gcd-terms t2 (remainder-terms t1 t2))))

  ;;   (if (same-variable? (variable p1)
  ;;                       (variable p2))
  ;;       (make-poly (variable p1)
  ;;                  (gcd-terms (term-list p1) (term-list p2)))
  ;;       (error "Polynomials are not of same variable (GCD-POLY)")))
  
  ;; Ex 2.96: Pseudoremainder-terms & gcd-poly
  (define (pseudoremainder-terms L1 L2)
    (define (scale-terms L scalar)
      (if (empty-termlist? L)
          L
          (adjoin-term (make-term (order (first-term L))
                                  (mul scalar (coeff (first-term L))))
                       (scale-terms (rest-terms L) scalar))))
    (let ((c (coeff (first-term L2)))
          (o1 (order (first-term L1)))
          (o2 (order (first-term L2))))
      (cadr (div-terms (scale-terms L1
                                    (expt c (+ 1 o1 (- o2))))
                       L2))))

  (define (factorize-coefficients terms)
    ;; Divide all terms of a term-list by their common (integer) GCD
    (let ((coeffs (map-terms coeff terms))
          (orders (map-terms order terms)))
      (let ((factor (apply gcd-many coeffs)))
        (make-term-list orders
                        (map (lambda (c) (/ c factor)) coeffs)))))

  (define (map-terms proc terms)
    ;; apply a procedure to every term in a term-list
    (if (null? terms)
        terms
        (cons (proc (first-term terms))
              (map-terms proc (rest-terms terms)))))
  
  (define (make-term-list orders coeffs)
    ;; Create a term-list from a list of orders and coefficients
    (if (null? orders)
        (the-empty-termlist)
        (adjoin-term (make-term (car orders) (car coeffs))
                     (make-term-list (cdr orders) (cdr coeffs)))))
  
  (define (gcd-terms L1 L2)
    ;; Return the GCD of two term-lists with the smallest integer
    ;; coefficients
    
    (define (gcd-terms-unfactored t1 t2)
      ;; Calculate the unfactored (large coeffs) GCD using Euler's method
      (if (empty-termlist? t2)
          t1
          (gcd-terms-unfactored t2 (pseudoremainder-terms t1 t2))))

    (factorize-coefficients (gcd-terms-unfactored L1 L2)))

  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (make-poly (variable p1)
                   (gcd-terms (term-list p1) (term-list p2)))
        (error "Polynomials are not of same variable (GCD-POLY)")))
  
  ;; Ex. 2.97a
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
    
  ;; interface to rest of the system
    (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  
  ;; Ex 2.87
  (put '=zero? '(polynomial)
     (lambda (p) (empty-termlist? (term-list p))))
  
  ;; Ex 2.88
  (put 'negate '(polynomial)
       (lambda (p) (tag (negate-poly p))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 (negate-poly p2)))))
  
  ;; Ex 2.91
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (map tag (div-poly p1 p2))))
  
  ;; Ex 2.92
  (put 'expand '(polynomial) expand-poly)
  
  ;; Ex 2.93 support
  (put 'equ? '(polynomial polynomial) equ-poly?)

  ;; Ex 2.94
  (put 'greatest-common-divisor '(polynomial polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  
  ;; Ex 2.97
  (put 'reduce '(polynomial polynomial) reduce-poly)
  
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(install-polynomial-package)

(define (expand x) (apply-generic 'expand x))

(define (divide-unit-test)
  
  (define p3 (make-polynomial 'x '((2 1) (0 -9))))
  (define p4 (make-polynomial 'x '((1 1) (0 -3))))
  (define p1 (make-polynomial 'x '((3 1) (0 -1))))
  (define p2 (make-polynomial 'x '((1 1) (0 -1))))
  (define q1 (make-polynomial 'y (list (list 3 p2) (list 1 p1) (list 0 4))))
  (define q2 (make-polynomial 'y (list (list 3 p4) (list 2 p3) (list 1 4))))
  
  (div p1 p2)
  (div p3 p4))
  

(put-coercion 'scheme-number 'polynomial
              (lambda (n) (make-polynomial 'x (list (list 0 (contents n))))))
