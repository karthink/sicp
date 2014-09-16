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

;;; (define (f x y . z) <body>)

;;; the procedure f can be called with two or more arguments. If we
;;; evaluate

;;; (f 1 2 3 4 5 6)

;;; then in the body of f, x will be 1, y will be 2, and z will be the
;;; list (3 4 5 6). Given the definition

;;; (define (g . w) <body>)

;;; the procedure g can be called with zero or more arguments. If we
;;; evaluate

;;; (g 1 2 3 4 5 6)

;;; then in the body of g, w will be the list (1 2 3 4 5 6).

;;; Use this notation to write a procedure same-parity that takes one
;;; or more integers and returns a list of all the arguments that have
;;; the same even-odd parity as the first argument. For example,

(define (same-parity x . y)
  (let ((parity (if (even? x) even? odd?)))
    (define (filter seq)
      (if (null? seq)
          seq
          (let ((y1 (car seq)))
            (if (parity y1)
                (cons y1 (filter (cdr seq)))
                (filter (cdr seq))))))
    (filter y)))

(same-parity 1 2 3 4 5 6 7)             ;(3 5 7)
(same-parity 2 3 4 5 6 7)               ;(4 6)

;;; This solution is complicated more than it needs to be by the fact
;;; that you cannot call same-parity recursively, as it reads in y as
;;; a list but cannot be called with a list. Hence the "filter"
;;; function to help out.

;;-------------
;;EXERCISE 2.21
;;-------------

;;; The procedure square-list takes a list of numbers as argument and
;;; returns a list of the squares of those numbers.

;;; (square-list (list 1 2 3 4))
;;; (1 4 9 16)

;;; Here are two different definitions of square-list. Complete both
;;; of them by filling in the missing expressions:


(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list (cdr items)))))

(define (maps proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (maps proc (cdr items)))))

(define (square-list-map items)
  (maps square items ))

;;-------------
;;EXERCISE 2.22
;;-------------

;;; Louis Reasoner tries to rewrite the first square-list procedure of
;;; Exercise 2-21 so that it evolves an iterative process:

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

;;; Unfortunately, defining square-list this way produces the answer
;;; list in the reverse order of the one desired. Why?

;;; Louis then tries to fix his bug by interchanging the arguments to cons:

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

;;; This doesn't work either. Explain.

;;; Successively stripping cars off a list and consing them to nil
;;; will build a list in the reverse order. See the solution to (and
;;; explanation of) reverse above, in Ex. 2.18.

;;; The "fix" does not create a list structure at all, at least in the
;;; usual sense. It conses a nil to the square of the first element of
;;; items, creating (() . (car items)). It then conses this to (cadr
;;; items), creating ((() . (car items)) . (cadr items)), and so on.
;;; Viewed as a tree, it creates a mirror-image of items.

;;-------------
;;EXERCISE 2.23
;;-------------

;;; The procedure for-each is similar to map. It takes as arguments a
;;; procedure and a list of elements. However, rather than forming a
;;; list of the results, for-each just applies the procedure to each
;;; of the elements in turn, from left to right. The values returned
;;; by applying the procedure to the elements are not used at all -
;;; for-each is used with procedures that perform an action, such as
;;; printing. For example,


(my-for-each (lambda (x) (newline) (display x))
             (list 57 321 88))

;;; should print 57, 321 and 88. The value returned by the call to
;;; for-each should be true. Give an implementation of for-each.

(define (my-for-each proc lst)
  (cond ((not (null? lst))
         (proc (car lst))
         (my-for-each proc (cdr lst)))))

;;-------------
;;EXERCISE 2.24
;;-------------

;;; Suppose we evaluate the expression (list 1 (list 2 (list 3 4))).
;;; Give the result printed by the interpreter, the corresponding
;;; box-and-pointer structure, and the interpretation of this as a
;;; tree (as in Figure 2-6).

;;; The result will be (1 (2 (3 4)))

;;; Tree:

;;;             X
;;;            / \
;;;           /   \
;;;          /     X
;;;         /     / \-
;;;        /     /    \
;;;       1     2      X
;;;                   / \
;;;                  /   \
;;;                 3     4
;;;
;;; Box & Pointer:
;;; Difficult to draw here. But take the tree, convert every X to a
;;; cons-cell.

;;-------------
;;EXERCISE 2.25
;;-------------

;;; Give combinations of cars and cdrs that will pick 7 from each of
;;; the following lists. Represent the list as l.

;;; (1 3 (5 7) 9)
;;; (car (cdr (car (cdr (cdr l)))))

;;; ((7))
;;; (car (car l))

;;; (1 (2 (3 (4 (5 (6 7))))))
;;; (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l))))))))))))

;;-------------
;;EXERCISE 2.26
;;-------------

;;; Suppose we define x and y to be two lists:

(define x (list 1 2 3))
(define y (list 4 5 6))

;;; What result is printed by the interpreter in response to
;;; evaluating each of the following expressions?

(append x y)
'(1 2 3 4 5 6)

(cons x y)
'((1 2 3) 4 5 6)

(list x y)
'((1 2 3) (4 5 6))

;;-------------
;;EXERCISE 2.27
;;-------------

;;; Modify your reverse procedure of Exercise 2-18 to produce a
;;; deep-reverse procedure that takes a list as argument and returns
;;; as its value the list with its elements reversed and with all
;;; sublists deep-reversed as well. For example,

;;; (define x (list (list 1 2) (list 3 4)))

;;; (deep-reverse x)
;;; ((4 3) (2 1))

(define (deep-reverse l)
  (define (iter lst res)
    (cond ((null? lst) res)
          ((not (pair? lst)) lst)
          (else (iter (cdr lst)
                      (cons (deep-reverse (car lst)) res)))))
  (iter l nil))

;;; deep-reverse of () is ().
;;; deep-reverse of an atom is the atom itself
;;; deep-reverse of a pair is the deep-reverse of the cdr followed by
;;; the deep-reverse of the car

;;-------------
;;EXERCISE 2.28
;;-------------

;;; Write a procedure fringe that takes as argument a tree
;;; (represented as a list) and returns a list whose elements are all
;;; the leaves of the tree arranged in left-to-right order. For
;;; example,

;;; (define x (list (list 1 2) (list 3 4)))

;;; (fringe x)
;;; (1 2 3 4)

;;; (fringe (list x x))
;;; (1 2 3 4 1 2 3 4)

(define (fringe lst)
  (cond ((null? lst) nil)
        ((not (pair? (car lst))) lst)
        (else (append (fringe (car lst)) (fringe (cdr lst))))))

;;; The fringe of nil is nil
;;; The fringe of an atom is atom
;;; The fringe of a list/tree is the fringe of the cdr appended to the
;;; fringe of the car

;;-------------
;;EXERCISE 2.29
;;-------------

;;; A binary mobile consists of two branches, a left branch and a
;;; right branch. Each branch is a rod of a certain length, from which
;;; hangs either a weight or another binary mobile. We can represent a
;;; binary mobile using compound data by constructing it from two
;;; branches (for example, using list):

(define (make-mobile left right)
  (list left right))

;;; A branch is constructed from a length (which must be a number)
;;; together with a structure, which may be either a number
;;; (representing a simple weight) or another mobile:

(define (make-branch length structure)
  (list length structure))

;;; Write the corresponding selectors left-branch and right-branch,
;;; which return the branches of a mobile, and branch-length and
;;; branch-structure, which return the components of a branch.

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))  

;;; Using your selectors, define a procedure total-weight that returns
;;; the total weight of a mobile.

(define (total-weight mobile)
  (define (branch-weight branch)
  (let ((s (branch-structure branch)))
    (cond ((not (pair? s)) s)
          (else (total-weight s)))))
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

;;; 2. If mobile 

;; (setq last-kbd-macro
;;    [?\M-d ?\C-x ?r ?n ?a ?\C-x ?r ?n ?b ?\C-f ?\C-x ?r ?n ?b ?\C-  ?\C-a ?\C-w ?\C-d ?\C-d ?\C-b ?\C-d ?\M-o ?\M-o ?\; ?- tab ?e ?x ?e ?r ?c ?i ?s ?e ?  ?\C-x ?r ?i ?a ?\C-f ?. ?\C-x ?r ?i ?b tab ?\C-k ?\C-n ?\; ?\; ?\; ?  ?\M-q ?\M-\]])
