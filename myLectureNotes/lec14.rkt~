#lang racket

(define remainder
  (λ (m n)
    (cond
      ((< m n) m)
      (else (remainder (- m n) n))
      )))

(define gcd
  (λ (a b)
    (cond
      ((zero? b) a)
      (else (gcd b (remainder a b)))
      )))

;; Registerized GCD
(define remainder*
  (λ (m n)
    (cond
      ((< m n) m)
      (else (remainder* (- m n) n))
      )))

;(define gcd*
;  (λ (*a* *b*)
;    (cond
;      ((zero? *b*) *a*)
;      (else (let* ((*old-a* *a*)
;                   (*a* *b*)
;                   (*b* (remainder* *old-a* *b*)))
;            (gcd* *a* *b*))))))
;;We can skip doing RI because we do not have a continuation, so we do not need the helper functions
;and can skip the step, and we also do not have an apply-k, so we also skip that part
;So then we star the variables 
;No need to start the let in the zero case, because we are simply returning a value
;;Putting *a* or *b* first just breaks either way, because they both use each other. We can get rid of the * from let* which works, but
; we can add a temporary old-a* to the let* to make it work with let*
;We also have to make the variables down here 
;(let* ((*a* 15)
;       (*b* 24)) (gcd* *a* *b*))

(define *a* #f)
(define *b* #f)
(define *old-a* #f)

(define gcd*
  (λ ()
    (cond
      ((zero? *b*) *a*)
      (else (begin (set! *old-a* *a*)
                   (set! *a* *b*)
                   (set! *b* (remainder* *old-a* *b*))
                   (gcd*))))))
;Next we first get rid of all the parameters from the lambda and define the global variables, then we add the begin with the set!s, and we set the variables in the call
;Then we add all the set!s, and add the function call to the bottom of the begin, and then do a begin with set!s in the function call to test it
;(begin (set! *a* 15)
;       (set! *b* 24)
;       (gcd*))

