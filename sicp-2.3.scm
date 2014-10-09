;;-------------
;;EXERCISE 2.73
;;-------------

;;; Section 2-3-2 described a program that performs symbolic
;;; differentiation:


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
 
        (else (error "unknown expression type -- DERIV" exp))))

;;; We can regard this program as performing a dispatch on the type of
;;; the expression to be differentiated. In this situation the "type
;;; tag" of the datum is the algebraic operator symbol (such as +) and
;;; the operation being performed is deriv. We can transform this
;;; program into data-directed style by rewriting the basic derivative
;;; procedure as


(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))
 
(define (operator exp) (car exp))
 
(define (operands exp) (cdr exp))

;;; Explain what was done above. Why can't we assimilate the
;;; predicates number? and same-variable? into the data-directed
;;; dispatch?

;;; Ans: number? and variable? cannot be dispatched because they are
;;; predicates, not operations. We could add an entry to the table for
;;; dealing with numbers like so:

(define (deriv-number) 0)
(put 'number '(my-tag) deriv-number)

;;; Write the procedures for derivatives of sums and products, and the
;;; auxiliary code required to install them in the table used by the
;;; program above.

(define (install-deriv-sum-package)
  ;; internal procedures
  (define (deriv-sum operands)
    (let ((addend (car operands))
          (augend (cadr operands)))
      (make-sum (deriv (addend exp) var)
                (deriv (augend exp) var))))
  
  (define (deriv-product operands)
    (let ((multiplier (car operands))
          (multiplicand (cadr operands)))
      (make-sum 
       (make-product (multiplier exp)
                     (deriv (multiplicand exp) var))
       (make-product (deriv (multiplier exp) var)
                     (multiplicand exp)))))
  
  (define (make-sum addend augend)
    (cond ((and (number? addend) (number? augend)) (+ addend augend))
          ((=number? addend 0) augend)
          ((=number? augend 0) addend)
          (else (list '+ addend augend))))
  
  (define (make-product multiplier multiplicand)
    (cond ((and (number? multiplier) (number? multiplicand))
           (* multiplier multiplicand))
          ((or (=number? multiplier 0)
               (=number? multiplicand 0))
           0)
          ((=number? multiplier 1) multiplicand)
          ((=number? multiplicand 1) multiplier)
          (else (list '* multiplier multiplier))))
  
;;; Choose any additional differentiation rule that you like, such as
;;; the one for exponents (Exercise 2-56), and install it in this
;;; data-directed system.

  (define (make-exp operands)
    (let ((base (car operands))
          (exp (cadr operands)))
      (cond ((=number? base 1) 1)
            ((=number? exp 1) base)
            ((or (=number? exp 0) (=number? base 0)) 1)
            (else (list '** base exp)))))
  
  (define (deriv-exp operands)
    (let ((base (car operands))
          (exp (cadr operands)))
      (make-product exp
                    (make-product (make-exp base
                                            (make-sum (-1) exp))
                                  (deriv base var)))))
  (put 'deriv '* deriv-sum)
  (put 'deriv '+ deriv-product)
  (put 'deriv '** deriv-exp))


;;; In this simple algebraic manipulator the type of an expression is
;;; the algebraic operator that binds it together. Suppose, however,
;;; we indexed the procedures in the opposite way, so that the
;;; dispatch line in deriv looked like

((get (operator exp) 'deriv) (operands exp) var)

;;; What corresponding changes to the derivative system are required?
;;; None, except changing the put statement to (ex) (put '* 'deriv
;;; deriv-product)

;;-------------
;;EXERCISE 2.74
;;-------------

;;; Insatiable Enterprises, Inc., is a highly decentralized
;;; conglomerate company consisting of a large number of independent
;;; divisions located all over the world. The company's computer
;;; facilities have just been interconnected by means of a clever
;;; network-interfacing scheme that makes the entire network appear to
;;; any user to be a single computer. Insatiable's president, in her
;;; first attempt to exploit the ability of the network to extract
;;; administrative information from division files, is dismayed to
;;; discover that, although all the division files have been
;;; implemented as data structures in Scheme, the particular data
;;; structure used varies from division to division. A meeting of
;;; division managers is hastily called to search for a strategy to
;;; integrate the files that will satisfy headquarters' needs while
;;; preserving the existing autonomy of the divisions.


