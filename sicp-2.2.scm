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

;;; I was first inclined to try this:

(define (fringe lst)
  (cond ((null? lst) nil)
        ((not (pair? (car lst))) lst)
        (else (cons (fringe (car lst)) (fringe (cdr lst))))))

;;; But this simply reproduces the structure of lst. The reason is
;;; simple. If x is

(define x (cons 'y 'z)) ;(y . z)

;;; Then consing its car and cdr will give us a duplicate of x:

(cons (car x) (cdr x))  ;(y . z)

;;; The right way to flatten a tree into a list is to use append
;;; instead of cons.

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

(define (branch-weight branch)
  (let ((s (branch-structure branch)))
    (cond ((not (pair? s)) s)
          (else (total-weight s)))))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define level-1-mobile (make-mobile (make-branch 2 2) 
                                    (make-branch 1 1))) 
(define level-2-mobile (make-mobile (make-branch 3 level-1-mobile) 
                                    (make-branch 9 1))) 
(define level-3-mobile (make-mobile (make-branch 4 level-2-mobile) 
                                    (make-branch 8 2))) 

;;; A mobile is said to be balanced if the torque applied by its
;;; top-left branch is equal to that applied by its top-right branch
;;; (that is, if the length of the left rod multiplied by the weight
;;; hanging from that rod is equal to the corresponding product for
;;; the right side) and if each of the submobiles hanging off its
;;; branches is balanced. Design a predicate that tests whether a
;;; binary mobile is balanced.

(define (is-mobile? branch)
  (pair? (branch-structure branch)))

(define (branch-torque branch)
  (* (branch-weight branch) (branch-length branch)))

(define (balanced? mobile)
  (let ((lb (left-branch mobile))
        (rb (right-branch mobile)))
    (and  (= (branch-torque lb)
             (branch-torque rb))
          (if (is-mobile? lb)
              (balanced? (branch-structure lb))
              #t)
          (if (is-mobile? rb)
              (balanced? (branch-structure rb))
              #t))))

(define level-4-mobile (make-mobile (make-branch 1 level-3-mobile)
                                    (make-branch 4 6)))

;; (setq last-kbd-macro
;;    [?\M-d ?\C-x ?r ?n ?a ?\C-x ?r ?n ?b ?\C-f ?\C-x ?r ?n ?b ?\C-  ?\C-a ?\C-w ?\C-d ?\C-d ?\C-b ?\C-d ?\M-o ?\M-o ?\; ?- tab ?e ?x ?e ?r ?c ?i ?s ?e ?  ?\C-x ?r ?i ?a ?\C-f ?. ?\C-x ?r ?i ?b tab ?\C-k ?\C-n ?\; ?\; ?\; ?  ?\M-q ?\M-\]])

;;; Suppose we change the representation of mobiles so that the
;;; constructors are

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

;;; How much do you need to change your programs to convert to the new
;;; representation?

;;; We need to replace cadr in all the selectors with cdr. That's it.
;;; None of the above functions are affected because they manipulate
;;; mobiles only with these selectors.

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))  

;;-------------
;;EXERCISE 2.30
;;-------------

;;; Define a procedure square-tree analogous to the square-list
;;; procedure of Exercise 2-21. That is, square-list should behave as
;;; follows:

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

;(1 (4 (9 16) 25) (36 49))

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

;;; OR

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

;;-------------
;;EXERCISE 2.31
;;-------------

;;; Abstract your answer to Exercise 2-30 to produce a procedure
;;; tree-map with the property that square-tree could be defined as

(define (square-tree tree) (tree-map square tree))

(define (tree-map f tree)
  (cond ((null? tree) nil)
        ((pair? tree) (cons (tree-map f (car tree))
                            (tree-map f (cdr treee))))
        (else (f tree))))

;;-------------
;;EXERCISE 2.32
;;-------------

;;; We can represent a set as a list of distinct elements, and we can
;;; represent the set of all subsets of the set as a list of lists.
;;; For example, if the set is (1 2 3), then the set of all subsets is
;;; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)). Complete the following
;;; definition of a procedure that generates the set of subsets of a
;;; set and give a clear explanation of why it works:

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (lst) (cons (car s) lst))
                          rest)))))

;;; The missing piece (procedure that map maps) is

;;; (lambda (lst) (cons (car s) lst))

;;; Why it works: The list of all subsets of a list is:
;;; 1. The list of all subsets of the list without the first element,
;;; appended to 
;;; 2. The list of all subsets of the list with the first element,
;;; 3. Where subsets of the empty list is (list '()). 

;;-------------
;;EXERCISE 2.33
;;-------------

;;; Fill in the missing expressions to complete the following
;;; definitions of some basic list-manipulation operations as
;;; accumulations:

;;; Some primitives
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence)) (cons (car sequence)
                                          (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))


(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (inc y)) 0 sequence))

;;-------------
;;EXERCISE 2.34
;;-------------

;;; Evaluating a polynomial in x at a given value of x can be
;;; formulated as an accumulation. We evaluate the polynomial

an r^n + an−1 r^n−1 + ... + a1 r + a0

;; using Hroner's rule. We start with an, multiply by x, add an−1,
;; multiply by x, and so on, until we reach a0.

;;; Fill in the following template to produce a procedure that
;;; evaluates a polynomial using Horner's rule. Assume that the
;;; coefficients of the polynomial are arranged in a sequence, from a0
;;; through an.

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;;-------------
;;EXERCISE 2.35
;;-------------

;;; Redefine count-leaves from section 2-2-2 as an accumulation:

;; (define (count-leaves t)
;;   (accumulate 'your 'answer (map 'your 'answer)))

;;; This one will melt your brain.
(define (count-leaves t)
  (accumulate + 0 (map (lambda (element)
                         (if (pair? element)
                             (count-leaves element)
                             1))
                       t)))

;;-------------
;;EXERCISE 2.36
;;-------------

;;; The procedure accumulate-n is similar to accumulate except that it
;;; takes as its third argument a sequence of sequences, which are all
;;; assumed to have the same number of elements. It applies the
;;; designated accumulation procedure to combine all the first
;;; elements of the sequences, all the second elements of the
;;; sequences, and so on, and returns a sequence of the results. For
;;; instance, if s is a sequence containing four sequences, ((1 2 3)
;;; (4 5 6) (7 8 9) (10 11 12)), then the value of (accumulate-n + 0
;;; s) should be the sequence (22 26 30). Fill in the missing
;;; expressions in the following definition of accumulate-n:

;; (define (accumulate-n op init seqs)
;;   (if (null? (car seqs))
;;       nil
;;       (cons (accumulate op init 'your-answer)
;;             (accumulate-n op init 'your-answer))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;;-------------
;;EXERCISE 2.37
;;-------------

;;; Suppose we represent vectors v = (v_i) as sequences of numbers,
;;; and matrices m = (m_(ij)) as sequences of vectors (the rows of the
;;; matrix). For example, the matrix

;; [ 1 2 3 4 ] 
;; [ 4 5 6 6 ]
;; [ 6 7 8 9 ]

;;; is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8 9)).
;;; With this representation, we can use sequence operations to
;;; concisely express the basic matrix and vector operations. These
;;; operations (which are described in any book on matrix algebra) are
;;; the following:

;; (dot-product v w) returns the sum_i vi wi
;; (matrix-*-vector m w) returns the vector t, where ti = sum_j mij vj
;; (matrix-*-matrix m n) returns the matrix p, where pij = sum_k mik nkj
;; (transpose m) returns the matrix n, where nij  =  mji

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;;; Without map:
(define (dot-product v w)
  (accumulate + 0
              (accumulate-n * 1 (list v w))))

;;; Fill in the missing expressions in the following procedures for
;;; computing the other matrix operations. (The procedure accumulate-n
;;; is defined in Exercise 2-36.)

(define (matrix-*-vector m v)
  (map (lambda (row)
         (dot-product v row)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (map (lambda (col)
                  (dot-product row col))
                cols))
         m)))

;;; OR 

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (matrix-*-vector cols row))
         m)))
