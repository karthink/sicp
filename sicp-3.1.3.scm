;;-------------
;;EXERCISE 3.7:
;;-------------

;;; Consider the bank account objects created by make-account, with
;;; the password modification described in Exercise 3.3. Suppose that
;;; our banking system requires the ability to make joint accounts.
;;; Define a procedure make-joint that accomplishes this. Make-joint
;;; should take three arguments. The first is a password-protected
;;; account. The second argument must match the password with which
;;; the account was defined in order for the make-joint operation to
;;; proceed. The third argument is a new password. Make-joint is to
;;; create an additional access to the original account using the new
;;; password. For example, if peter-acc is a bank account with
;;; password open-sesame, then

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

;;; will allow one to make transactions on peter-acc using the name
;;; paul-acc and the password rosebud. You may wish to modify your
;;; solution to Exercise 3.3 to accommodate this new feature.

;;; Here's the solution to exercise 3.3
(define (make-account balance passwd)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((wrong-passwd 0)
        (passwds (list passwd)))
    (define (dispatch p m)
      (if (member p passwds)
          (begin (set! wrong-passwd 0)
                 (cond ((eq? m 'withdraw) withdraw)
                       ((eq? m 'deposit) deposit)
                       ((eq? m 'add-passwd) (lambda (new-pass)
                                              (set! passwds
                                                    (cons new-pass passwds))))
                       (else (error "Unknown request: MAKE-ACCOUNT"
                                    m))))
          (if (< wrong-passwd 7)
              (lambda (m)
                (set! wrong-passwd (+ wrong-passwd 1))
                "Incorrect password")
              (lambda (m) (call-the-cops)))))
    dispatch))

(define (call-the-cops) "Cops called")

(define (make-joint old-acc old-pass new-pass)
  ((old-acc old-pass 'add-passwd) new-pass)
  old-acc)

;;; The problem with this solution is that since password verification is now based on a list membership test, old-acc can also use the new-pass. :(

;;; Here's a better solution without this problem that does not
;;; require make-account to be modified at all

(define (make-joint old-acc old-pass new-pass)
  (lambda (p m)
    (if (eq? p new-pass)
        (old-acc old-pass m)
        (old-acc '#f m))))

;;-------------
;;EXERCISE 3.8:
;;-------------

;;; When we defined the evaluation model in Section 1.1.3, we said
;;; that the first step in evaluating an expression is to evaluate its
;;; subexpressions. But we never specified the order in which the
;;; subexpressions should be evaluated (e.g., left to right or right
;;; to left). When we in- troduce assignment, the order in which the
;;; arguments to a procedure are evaluated can make a difference to
;;; the result. Define a simple procedure f such that evaluating

(+ (f 0) (f 1))

;;; will return 0 if the arguments to + are evaluated from left to
;;; right but will return 1 if the arguments are evaluated from right
;;; to left.

(define f (let ((state 1))
            (lambda (x)
              (if (= x 0)
                  (set! state 0))
              state)))

(+ (f 0) (f 1))                         ;=> 1
(+ (f 1) (f 0))                         ;=> 0

