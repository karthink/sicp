;;------------
;;EXERCISE 3.1
;;------------

;;; An accumulator is a procedure that is called repeatedly with a
;;; single numeric argument and accumulates its arguments into a sum.
;;; Each time it is called, it returns the currently accumulated sum.
;;; Write a procedure make-accumulator that generates accumulators,
;;; each maintaining an independent sum. The input to make-accumulator
;;; should specify the initial value of the sum. For example

(define A (make-accumulator 5))
(A 10)
15
(A 10)
25

(define (make-accumulator sum)
  (lambda (add)
    (begin
      (set! sum (+ sum add))
      sum)))

;;-------------
;;EXERCISE 3.2:
;;-------------

;;; In software-testing applications, it is useful to be able to count
;;; the number of times a given procedure is called during the course
;;; of a computation. Write a pro- cedure make-monitored that takes as
;;; input a procedure, f,that itself takes one input. The result
;;; returned by make- monitored is a third procedure, say mf, that
;;; keeps track of the number of times it has been called by
;;; maintaining an internal counter. If the input to mf is the special
;;; symbol how-many-calls?, then mf returns the value of the counter.
;;; If the input is the special symbol reset-count, then mf re- sets
;;; the counter to zero. For any other input, mf returns the result of
;;; calling f on that input and increments the counter. For instance,
;;; we could make a monitored version of the sqrt procedure:

(define s (make-monitored sqrt))
(s 100)
10
(s 'how-many-calls?)
1

(define (make-monitored f)
  (let ((number-of-calls 0))
    (lambda (var)
      (cond ((eq? var 'how-many-calls?)
             number-of-calls)
            ((eq? var 'reset-count)
             (set! number-of-calls 0))
            (else (set! number-of-calls (+ number-of-calls 1))
                  (f var))))))

;;-------------
;;EXERCISE 3.3:
;;-------------

;;; Modify the make-account procedure so that it creates
;;; password-protected accounts. That is, make-account should take a
;;; symbol as an additional argument, as in

(define acc (make-account 100 'secret-password))

;;; The resulting account object should process a request only if it
;;; is accompanied by the password with which the account was created,
;;; and should otherwise return a complaint:

((acc 'secret-password 'withdraw) 40)
60
((acc 'some-other-password 'deposit) 50)
"Incorrect password"

;;; The only change is to dispatch. See exercise 3.4 for the procedure

;;-------------
;;EXERCISE 3.4:
;;-------------

;;; Modify the make-account procedure of Exercise 3.3 by adding
;;; another local state variable so that, if an account is accessed
;;; more than seven consecutive times with an incorrect password, it
;;; invokes the procedure call-the-cops.

(define (make-account balance passwd)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((wrong-passwd 0))
    (define (dispatch p m)
      (if (eq? p passwd)
          (begin (set! wrong-passwd 0)
                 (cond ((eq? m 'withdraw) withdraw)
                       ((eq? m 'deposit) deposit)
                       (else (error "Unknown request: MAKE-ACCOUNT"
                                    m))))
          (if (< wrong-passwd 7)
              (lambda (m)
                (set! wrong-passwd (+ wrong-passwd 1))
                "Incorrect password")
              (lambda (m) (call-the-cops)))))
    dispatch))

(define (call-the-cops) "Cops called")

;;; Question: Why doesn't this work if the let is inside the definition of dispatch?
;;; Ans: Because the let clause is then run on every call to dispatch, resetting wrong-passwd to 0 each time.
