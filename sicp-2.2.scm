;;----------
;;PRIMITIVES
;;----------
(define nil '())

;;-------------
;;EXERCISE 2.17
;;-------------

;;; Define a procedure last-pair that returns the list that contains
;;; only the last element of a given (nonempty) list:

(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

;;-------------
;;EXERCISE 2.18
;;-------------

;;; Define a procedure reverse that takes a list as argument and
;;; returns a list of the same elements in reverse order:

;;; Stripping successive cars and consing it onto an empty list will
;;; result in a reversed list.
(define (reverse l)
  (define (iter the-list result)
    (if (null? the-list)
        result
        (iter (cdr the-list)  (cons (car the-list) result))))
  (iter l '()))


;;; Here's a stupid way of doing it.
(define (reverse l)
  (define (all-but-last l)
  (if (null? (cdr l))
      '()
      (cons (car l) (all-but-last (cdr l)))))
  
  (if (null? l)
      '()
      (cons (car (last-pair l))
            (reverse (all-but-last l)))))


;;-------------
;;EXERCISE 2.19
;;-------------

;;; Consider the change-counting program of section 1-2-2. It would be
;;; nice to be able to easily change the currency used by the program,
;;; so that we could compute the number of ways to change a British
;;; pound, for example. As the program is written, the knowledge of
;;; the currency is distributed partly into the procedure
;;; first-denomination and partly into the procedure count-change
;;; (which knows that there are five kinds of U.S. coins). It would be
;;; nicer to be able to supply a list of coins to be used for making
;;; change.

;;; We want to rewrite the procedure cc so that its second argument is
;;; a list of the values of the coins to use rather than an integer
;;; specifying which coins to use. We could then have lists that
;;; defined each kind of currency:

(define us-coins (list 50 25 10 5 1))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))

;;; To do this will require changing the program cc somewhat. It will
;;; still have the same form, but it will access its second argument
;;; differently, as follows:

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

;;; Define the procedures first-denomination,
;;; except-first-denomination, and no-more? in terms of primitive
;;; operations on list structures. Does the order of the list
;;; coin-values affect the answer produced by cc? Why or why not?
  
(define (no-more? coin-values) (null? coin-values))
(define (first-denomination coin-values)
  (car coin-values))
(define (except-first-denomination coin-values)
  (cdr coin-values))

;;; The order of the list does not matter, because cc will eventually
;;; run through all possibilities.

;;-------------
;;EXERCISE 2.20
;;-------------

;;; The procedures +, *, and list take arbitrary numbers of arguments.
;;; One way to define such procedures is to use define with
;;; dotted-tail notation . In a procedure definition, a parameter list
;;; that has a dot before the last parameter name indicates that, when
;;; the procedure is called, the initial parameters (if any) will have
;;; as values the initial arguments, as usual, but the final
;;; parameter's value will be a list of any remaining arguments. For
;;; instance, given the definition

(define (f x y . z) <body>)

;;; the procedure f can be called with two or more arguments. If we
;;; evaluate

(f 1 2 3 4 5 6)

;;; then in the body of f, x will be 1, y will be 2, and z will be the
;;; list (3 4 5 6). Given the definition

(define (g . w) <body>)

;;; the procedure g can be called with zero or more arguments. If we
;;; evaluate

(g 1 2 3 4 5 6)

;;; then in the body of g, w will be the list (1 2 3 4 5 6).

;;; Use this notation to write a procedure same-parity that takes one
;;; or more integers and returns a list of all the arguments that have
;;; the same even-odd parity as the first argument. For example,

(define (same-parity x . y)
  'your-code-here)

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)


