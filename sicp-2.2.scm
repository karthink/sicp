;;----------
;;PRIMITIVES
;;----------
(define (compose f g) (lambda (x) ( f (g x))))
(define (inc x)  (+ x 1))
(define nil '())
(define identity (lambda (entity) entity))

;;; sequence operations
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

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

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

;;; an r^n + an−1 r^n−1 + ... + a1 r + a0

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

;;; The inner (map (lambda (col) ...) cols) is (matrix-*-vector cols row):

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (matrix-*-vector cols row))
         m)))

;;-------------
;;EXERCISE 2.38
;;-------------

;;; The accumulate procedure is also known as fold-right, because it
;;; combines the first element of the sequence with the result of
;;; combining all the elements to the right. There is also a
;;; fold-left, which is similar to fold-right, except that it combines
;;; elements working in the opposite direction:

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;;; What are the values of

(define fold-right accumulate)
(fold-right / 1 (list 1 2 3))           ;3/2

(fold-left / 1 (list 1 2 3))            ;1/6

(fold-right list nil (list 1 2 3))      ;(1 (2 (3 ())))

(fold-left list nil (list 1 2 3))       ;(((() 1) 2) 3)

;;; Give a property that op should satisfy to guarantee that
;;; fold-right and fold-left will produce the same values for any
;;; sequence.

;;; op is a two-argument procedure; it should be commutative:
;;; (= (op y x) (op x y))

(fold-right * 1 (list 1 2 3))
(fold-left * 1 (list 1 2 3))

;;-------------
;;EXERCISE 2.39
;;-------------

;;; Complete the following definitions of reverse (Exercise 2-18) in
;;; terms of fold-right and fold-left from Exercise 2-38:

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))
 
(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(accumulate append
            nil
            (map (lambda (i)
                   (map (lambda (j)
                          (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 6)))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove x lst)
  (filter (lambda (y) (not (= y x))) lst))

;;-------------
;;EXERCISE 2.40
;;-------------

;;; Define a procedure unique-pairs that, given an integer n,
;;; generates the sequence of pairs (i,j) with 1≤j<i≤n. Use
;;; unique-pairs to simplify the definition of prime-sum-pairs given
;;; above.

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

;;-------------
;;EXERCISE 2.41
;;-------------

;;; Write a procedure to find all ordered triples of distinct positive
;;; integers i, j, and k less than or equal to a given integer n that
;;; sum to a given integer s.


(define (same-sum-triplets s N)
  (define (unique-triplets n)
    (flatmap (lambda (i)
               (flatmap (lambda (j)
                          (map (lambda (k) (list i j k))
                               (enumerate-interval 1 (- j 1))))
                        (enumerate-interval 1 (- i 1))))
             (enumerate-interval 1 n)))
  (define (sum-to-s? triplet)
    (= (+ (car triplet) (cadr triplet) (caddr triplet)) s))
  
  (filter sum-to-s? (unique-triplets N)))

;;-------------
;;EXERCISE 2.42
;;-------------

;;; The ``eight-queens puzzle'' asks how to place eight queens on a
;;; chessboard so that no queen is in check from any other (i.e., no
;;; two queens are in the same row, column, or diagonal).

;;; One way to solve the puzzle is to work across the board, placing a
;;; queen in each column. Once we have placed k−1 queens, we must
;;; place the kth queen in a position where it does not check any of
;;; the queens already on the board. We can formulate this approach
;;; recursively: Assume that we have already generated the sequence of
;;; all possible ways to place k−1 queens in the first k−1 columns of
;;; the board. For each of these ways, generate an extended set of
;;; positions by placing a queen in each row of the kth column. Now
;;; filter these, keeping only the positions for which the queen in
;;; the kth column is safe with respect to the other queens. This
;;; produces the sequence of all ways to place k queens in the first k
;;; columns. By continuing this process, we will produce not only one
;;; solution, but all solutions to the puzzle.

;;; We implement this solution as a procedure queens, which returns a
;;; sequence of all solutions to the problem of placing n queens on an
;;; n x n chessboard. queens has an internal procedure queen-cols that
;;; returns the sequence of all ways to place queens in the first k
;;; columns of the board.

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;; (define (queens board-size)
;;   (define (queen-cols k)
;;     (if (= k 0)
;;         (list empty-board)
;;         (filter
;;          (lambda (positions) (safe? k positions))
;;          (flatmap
;;           (lambda (new-row)
;;             (map (lambda (rest-of-queens)
;;                    (adjoin-position new-row k rest-of-queens))
;;                  (queen-col)))
;;           (lambda (rest-of-queens)
;;             (map (lambda (new-row)
;;                    (adjoin-position new-row k rest-of-queens))
;;                  (enumerate-interval 1 board-size)))
;;           (queen-cols (- k 1))))))
;;   (queen-cols board-size))

;;; In this procedure rest-of-queens is a way to place k−1 queens in
;;; the first k−1 columns, and new-row is a proposed row in which to
;;; place the queen for the kth column. Complete the program by
;;; implementing the representation for sets of board positions,
;;; including the procedure adjoin-position, which adjoins a new
;;; row-column position to a set of positions, and empty-board, which
;;; represents an empty set of positions. You must also write the
;;; procedure safe?, which determines for a set of positions, whether
;;; the queen in the kth column is safe with respect to the others.
;;; (Note that we need only check whether the new queen is safe---the
;;; other queens are already guaranteed safe with respect to each
;;; other.)

;;; The queens procedure does not make any assumptions about the data
;;; structure being used to hold a k-queen configuration. (Which is
;;; fantastic.) So here's a simple data structure that works for (say)
;;; a 5x5 board:

;;; rows (4 2 5 3 1)
;;;       ^
;;; cols  1 2 3 4 5 

;;; rows is a list of queen positions, the nth entry of which refers
;;; to a queen at ((nth rows n), n). The empty board is just '().

;;; Adjoining a queen to the list (for a 6x6 board) is done by consing
;;; a row position to the list:

;;; rows (cons 6 (4 2 5 3 1))
;;;      (6 4 2 3 5 1)
;;; cols  1 2 3 4 5 6      

;;; The configuration is valid if the queen at (1,6) is safe from the
;;; other queens' row or diagonal attacks. We don't need to check for
;;; attacks from the same column because the column index will be
;;; unique by definition.

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define empty-board nil)

(define (safe? k positions)
  (define (safe-kth? kth row-distance rest)
    (or (null? rest)
        (let ((next-queen-row (car rest)))
          (and  (not (= kth next-queen-row))
                (not (= (abs (- kth next-queen-row))
                        row-distance))
                (safe-kth? kth (+ row-distance 1) (cdr rest))))))
  (safe-kth? (car positions) 1 (cdr positions)))

;;; The above definitions do not actually need k at all. Here's a
;;; different, simpler (but equivalent) definition of safe? that does.

(define (safe? k positions)
  (let ((kth-queen (car positions))
        (rest-of-queens (cdr positions))
        (one-to-k (enumerate-interval 1 (- k 1))))
    (not (member kth-queen
                 (flatmap identity
                          (list rest-of-queens
                                (map - rest-of-queens one-to-k)
                                (map + rest-of-queens one-to-k)))))))


;;; Many of the possible solutions to the k-queens problem are
;;; transformations of each other. The same position corresponds to
;;; four positions when viewed from each side of the chess board as a
;;; base. There are four more transformations corresponding to the
;;; reverse of these, and even more from mirror images (which
;;; correspond to a rotation and a reversal, so they're taken care
;;; of.)

;;; Here is a function (uniquify) that eliminates all
;;; duplicates/transforms from a list of solutions and returns only
;;; truly unique ones. When applied to (queens 8), it reduces the
;;; number of solutions from 92 to 14.

(define (transformed? rows next)
  (define (flip r) (map (lambda (x) (- 9 x)) r))
  
  (define (rot90 rows) 
    (define (arrange-cdrwise lst idx)
      (define (careql? pr)
        (= (car pr) idx))
      (if (> idx 8)
          nil
          (cons (car (filter careql? lst))
                (arrange-cdrwise (filter (compose not careql?) lst)
                                 (+ idx 1)))))
    (let ((pairs (map (lambda (coord)
                        (list (cdr coord) (- 9 (car coord))))
                      (map cons (enumerate-interval 1 8) rows)))
          (one-to-k (enumerate-interval 1 8)))
      (map cadr (arrange-cdrwise pairs 1))))
  
  (define rot180 (compose reverse flip))
  
  (define rot270 (compose rot180 rot90))
  
  (let ((row-flip (flip rows))
        (row-rot180 (rot180 rows))
        (row-rot90 (rot90 rows))
        (row-rot270 (rot270 rows)))

    (accumulate (lambda (x y) (or x y))
                #f
                (map (lambda (tr) (equal? next tr))
                     (list row-flip row-rot180
                           row-rot90 row-rot270
                           (reverse row-rot90) (reverse row-rot270))))))

(define (uniquify positions)
  (accumulate (lambda (rows uniqued)
                (cons rows
                      (filter (lambda (pos)
                                (not (transformed? rows pos)))
                              uniqued)))
              nil
              positions))

;;-------------
;;EXERCISE 2.43
;;-------------

;;; Louis Reasoner is having a terrible time doing Exercise 2-42. His
;;; queens procedure seems to work, but it runs extremely slowly.
;;; (Louis never does manage to wait long enough for it to solve even
;;; the 6×6 case.) When Louis asks Eva Lu Ator for help, she points
;;; out that he has interchanged the order of the nested mappings in
;;; the flatmap, writing it as

(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
          (adjoin-position new-row k rest-of-queens))
        (queen-cols (- k 1))))
 (enumerate-interval 1 board-size))

;;; Explain why this interchange makes the program run slowly.
;;; Estimate how long it will take Louis's program to solve the
;;; eight-queens puzzle, assuming that the program in Exercise 2-42
;;; solves the puzzle in time T.

;;; The original code calls (queen-cols (- k 1)) exactly once for each
;;; k. Louis' code calls it board-size times. Let the board size be B.
;;; The amount of time it takes:

;; (queens N) calls (queen-cols N), which
;; calls (queen-cols (- N 1)) B times, each invocation of which
;; calls (queen-cols (- N 2)) B times, each invocation of which
;; calls (queen-cols (- N 3)) B times, and so on.
;; 
;; The total time taken is multiplied by a factor of:
;; 1 + B + B^2 + B^3 + ... B^N = (B^ (N+1) - 1) / (B - 1) ~ B^N = B^B,
;; since the board size equals the number of queens when queens is called.


;;------------------
;;A PICTURE LANGUAGE
;;------------------

;;-------------
;;EXERCISE 2.44
;;-------------

;;; Define the procedure up-split used by corner-split. It is similar
;;; to right-split, except that it switches the roles of below and
;;; beside.

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;;-------------
;;EXERCISE 2.45
;;-------------

;;; right-split and up-split can be expressed as instances of a
;;; general splitting operation. Define a procedure split with the
;;; property that evaluating

(define right-split (split beside below))
(define up-split (split below beside))

(define (split op1 op2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split op1 op2) painter (- n 1))))
          (op1 painter (op2 smaller smaller))))))

;;-------------
;;EXERCISE 2.46
;;-------------

;;; A two-dimensional vector v running from the origin to a point can
;;; be represented as a pair consisting of an x-coordinate and a
;;; y-coordinate. Implement a data abstraction for vectors by giving a
;;; constructor make-vect and corresponding selectors xcor-vect and
;;; ycor-vect. In terms of your selectors and constructor, implement
;;; procedures add-vect, sub-vect, and scale-vect that perform the
;;; operations vector addition, vector subtraction, and multiplying a
;;; vector by a scalar:

;;; Primitives:
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

;;; Operations
(define (combine-vect operation)
  (lambda (v1 v2)
    (make-vect (operation (xcor-vect v1)
                          (xcor-vect v2))
               (operation (ycor-vect v1)
                          (ycor-vect v2)))))

(define add-vect (combine-vect +))
(define sub-vect (combine-vect -))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))
;;-------------
;;EXERCISE 2.47
;;-------------

;;; Here are two possible constructors for frames:

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
 
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

;;; For each constructor supply the appropriate selectors to produce
;;; an implementation for frames.

;;; Selectors are origin-frame, edge1-frame and edge2-frame
;;; This works for both representations
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame) (cadr frame))

;;; This doesn't. First representation:
(define (edge2-frame frame) (caddr frame))

;;; Second representation:
(define (edge2-frame frame) (cddr frame))

;;-------------
;;EXERCISE 2.48
;;-------------

;;; A directed line segment in the plane can be represented as a pair
;;; of vectors---the vector running from the origin to the start-point
;;; of the segment, and the vector running from the origin to the
;;; end-point of the segment. Use your vector representation from
;;; Exercise 2-46 to define a representation for segments with a
;;; constructor make-segment and selectors start-segment and
;;; end-segment.

(define (make-segment v1 v2) (cons v1 v2))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))


;;-------------
;;EXERCISE 2.49
;;-------------

;;; Use segments->painter to define the following primitive painters:

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(let ((orig (make-vect 0 0))
      (x1y0 (make-vect 1 0))
      (x0y1 (make-vect 0 1))
      (x1y1 (make-vect 1 1)))
;;; The painter that draws the outline of the designated frame.
  (segments->painter (list (make-segment orig x1y0)
                           (make-segment x1y0 x1y1)
                           (make-segment x1y1 x0y1)
                           (make-segment x0y1 orig)))

;;; The painter that draws an ``X'' by connecting opposite corners of
;;; the frame.
  (segments->painter (list (make-segment orig x1y1)
                           (make-segment x0y1 x1y0))))

;;; The painter that draws a diamond shape by connecting the midpoints
;;; of the sides of the frame.
;;; This is tiresome
;; (segments->painter (list (make-segment (scale-vect 0.5 x1y0)
;;                                        (scale-vect 0.5 x0y1))
;;                          (make-segment (scale-vect 0.5 x1y0)
;;                                        (scale-vect 0.5 x1y1))
;;                          (make-segment (scale-vect 0.5 x1y0)
;;                                        (scale-vect 0.5 x1y1))
;;                          (make-segment (scale-vect 0.5 x1y0)
;;                                        (scale-vect 0.5 x0y1))))

;;; The wave painter.
;;; WAT

;;-------------
;;EXERCISE 2.50
;;-------------

;;; Define the transformation flip-horiz, which flips painters
;;; horizontally, and transformations that rotate painters
;;; counterclockwise by 180 degrees and 270 degrees.

(define (flip-hoiz painter)
  (transform-painter painter (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 0.0 1.0)))

(define (rotate180 painter)
  (tranform-painter painter (make-vect 1.0 1.0)
                    (make-vect 0.0 1.0)
                    (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (compose rotate90 rotate 180))

;;-------------
;;EXERCISE 2.51
;;-------------

;;; Define the below operation for painters. Below takes two painters
;;; as arguments. The resulting painter, given a frame, draws with the
;;; first painter in the bottom of the frame and with the second
;;; painter in the top. Define below in two different ways---first by
;;; writing a procedure that is analogous to the beside procedure
;;; given above, and again in terms of beside and suitable rotation
;;; operations (from Exercise 2-50).

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 1.0 0.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 0.0 1.0)
                              (make-vect 1.0 0.5))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))
  
(define (below painter1 painter2)
  (rotate270 (beside (rotate90 painter1) (rotate90 painter2))))

;;-------------
;;EXERCISE 2.52
;;-------------
;;; Skipped


;;---------------------------------------------------------------
;;SYMBOLIC DATA
;;---------------------------------------------------------------

;;-------------
;;EXERCISE 2.53
;;-------------

;;; What would the interpreter print in response to evaluating each of
;;; the following expressions?

(list 'a 'b 'c)      
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))           ;
(memq 'red '((red shoes) (blue socks))) ;
(memq 'red '(red shoes blue socks))

;;-------------
;;EXERCISE 2.54
;;-------------

;;; Two lists are said to be equal? if they contain equal elements
;;; arranged in the same order. For example,

(equal? '(this is a list) '(this is a list))

;;; is true, but

(equal? '(this is a list) '(this (is a) list))

;;; is false. To be more precise, we can define equal? recursively in
;;; terms of the basic eq? equality of symbols by saying that a and b
;;; are equal? if they are both symbols and the symbols are eq?, or if
;;; they are both lists such that (car a) is equal? to (car b) and
;;; (cdr a) is equal? to (cdr b). Using this idea, implement equal? as
;;; a procedure.

(define (equal? lst1 lst2)
  (if (and  (pair? lst1) (pair? lst2))
      (and (equal? (car lst1) (car lst2))
           (equal? (cdr lst1) (cdr lst2)))
      (eq? lst1 lst2)))

;;-------------
;;EXERCISE 2.55
;;-------------

;;; Eva Lu Ator types to the interpreter the expression

(car ''abracadabra)

;;; To her surprise, the interpreter prints back quote. Explain.

;;; ''abracadabra is (quote (quote abracadabra)), which is
;;; (quote 'abracadabra). The car of this is quote.

;;-------------------------
;;SYMBOLIC DIFFERENTITATION
;;-------------------------

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

;;-------------
;;EXERCISE 2.56
;;-------------

;;; Show how to extend the basic differentiator to handle more kinds
;;; of expressions. For instance, implement the differentiation rule

;;; d(u^n)dx=n u^n−1 (du/dx)

;;; by adding a new clause to the deriv program and defining
;;; appropriate procedures exponentiation?, base, exponent, and
;;; make-exponentiation. (You may use the symbol ** to denote
;;; exponentiation.) Build in the rules that anything raised to the
;;; power 0 is 1 and anything raised to the power 1 is the thing
;;; itself.

;;; Example: (** y 3)

(define (exponentiation? x) (eq? (car x) '**))
(define (base x) (cadr x))
(define (exponent x) (caddr x))

(define (make-exponentiation base exponent)
  (if (or (=number? base 1) (=number? exponent 0))
      1
      (list '** base exponent)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        
        ((exponentiation? exp)
         (let ((b (base exp))
               (e (exponent exp)))
           (make-product e
                         (make-product
                          (make-exponentiation b (make-sum -1 e))
                          (deriv b var)))))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

;;-------------
;;EXERCISE 2.57
;;-------------

;;;  Extend the differentiation program to handle sums and products of
;;;  arbitrary numbers of (two or more) terms. Then the last example
;;;  above could be expressed as

;;; (deriv '(* x y (+ x 3)) 'x)

;;; Try to do this by changing only the representation for sums and
;;; products, without changing the deriv procedure at all. For
;;; example, the addend of a sum would be the first term, and the
;;; augend would be the sum of the rest of the terms.

;;; sum? and product? will be unchanged, as will be addend and
;;; multiplier. We need to change augend, multiplicand, make-sum and
;;; make-product.

(define (augend x) (let ((rest (cddr x)))
                     (if (null? (cdr rest))
                         (car rest)
                         (cons '+ rest))))

(define (multiplicand x) (let ((rest (cddr x)))
                           (if (null? (cdr rest))
                               (car rest)
                               (cons '* rest))))
;;; OR

(define (augend x) (accumulate make-sum 0 (cddr x)))
(define (multiplicand x) (accumulate make-product 1 (cddr x)))

;;; Note: make-sum and make-product do not have to be touched.

;;-------------
;;EXERCISE 2.58
;;-------------

;;; Suppose we want to modify the differentiation program so that it
;;; works with ordinary mathematical notation, in which + and * are
;;; infix rather than prefix operators. Since the differentiation
;;; program is defined in terms of abstract data, we can modify it to
;;; work with different representations of expressions solely by
;;; changing the predicates, selectors, and constructors that define
;;; the representation of the algebraic expressions on which the
;;; differentiator is to operate.

;;;     Show how to do this in order to differentiate algebraic
;;;     expressions presented in infix form, such as (x + (3 * (x + (y
;;;     + 2)))). To simplify the task, assume that + and * always take
;;;     two arguments and that expressions are fully parenthesized.

;;;     The problem becomes substantially harder if we allow standard
;;;     algebraic notation, such as (x + 3 * (x + y + 2)), which drops
;;;     unnecessary parentheses and assumes that multiplication is
;;;     done before addition. Can you design appropriate predicates,
;;;     selectors, and constructors for this notation such that our
;;;     derivative program still works?


