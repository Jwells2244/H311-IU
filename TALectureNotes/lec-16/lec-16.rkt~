#lang racket

(require "mk-new.rkt")
(require "numbers-new.rkt")

; The Language of miniKanren

; We have expressions we call goals.  We can construct
; complex goals from simple goals.  We can use
; variables in goals.  Once we have all the goals laid
; out, we can ask miniKanren to find out if there
; exists a logical solution for all the goals.

; A goal is represented as a function that takes a
; substitution and returns a potentially infinite
; sequence of substitutions

; Here are some examples of goals.
;(== 3 2)
;fail
;(=/= 3 2)


; A substitution is like an environment where we
; look up a variable in an environment, but it's
; slightly different.  It differs because it can
; never fail to find a value. When it can't find a
; value for a variable it returns that
; variable. The empty substitution is represented
; by the empty list.

; A sequence is a list of a value and a potentially infinite
; sequence of substitutions.

; q is a (logic/meta) variable
;(run 1 q
;  (== 3 2))

; We have run!, which gives a cleaner output and
; it returns a procedure.
;(run! 1 q
;  (== 3 2))
;
;;(run 1 q
;;  (=/= 3 2))
;
;(run! 1 q
;  (=/= 3 2))
;
;(run! 2 q
;  (=/= 3 2))

; We can use an explicit conjunction between the
; goals.
;(run 2 q
;  (conj
;    (=/= 3 2)
;    (== 3 2)))

;(run! 2 q
;  (conj
;    (=/= 3 2)
;    (== 3 2)))
;
;; There is also an implicit conjunction between
;; the goals.
;;(run 2 q
;;  (=/= 3 2)
;;  (== 3 2))
;
;; There is an explicit disjunction between goals
;; defined using disj.
;(run! 2 q
;  (disj
;    (=/= 3 2)
;    (== 3 2)))
;
;; If we request more values than the number of
;; solutions, that's okay.
;(run! 3 q
;  (disj
;    (=/= 3 2)
;    (== 3 2)))

; To get all possible solutions we can use run*.
;(run* q
;  (disj
;    (=/= 3 2)
;    (== 3 2)))
;
;(run*! q
;  (disj
;    (=/= 3 2)
;    (== 3 2)))
;
;(run! 1 q
;  (disj
;    (=/= 3 2)
;    (=/= 5 6)))
;
;(run! 2 q
;  (disj
;    (=/= 3 2)
;    (=/= 5 6)))
;
;(run 2 q
;  (disj
;    (=/= 3 2)
;    (=/= 5 6)))

;(define next!
;  (run! 1 q
;    (disj
;      (=/= 3 2)
;      (=/= 5 6))))

; we can use the function returned by run! to query more
; answers.
;(next! 20)
;
;(run*! q
;  (disj
;    (=/= 3 2)
;    (=/= 5 6)))

;(run 1 q
;  (== q 3))
;
;(run! 1 q
;  (== q 3))
;
;(run* q
;  (== q 3))

; We can use fresh to create variables on the fly.
;(run 1 q
;  (fresh (x) (== x 4)))

; We can use more than one output variable.
(run! 1 (q r)
  (conj
    (== q 7)
    (== r 8)))

; more examples
(run! 10 (q r)
  (conj
    (=/= 3 q)
    (== 5 r)
    (== 6 q)))

(run! 10 q
  (fresh (x)
    (== x 42)))

(run! 10 (q r s)
  (fresh (x y z)
    (conj
      (== x 42)
      (disj (=/= y r)
            (== z 4))
      (== 1 q))))

; Can we write recursive programs in miniKanren?
(define append
  (λ (ls s)
    (match ls
      (`() s)
      (`(,a . ,d)
       (cons a (append d s))))))

(append '(a b c) '(d e))
(append '(a b c) 'd)

;; We can "miniKanrenize" our definition of append
;; to this:
(defrel (appendo ls s o)
  (disj
    (conj (== '() ls) (== s o))
    (fresh (a d res)
      (== `(,a . ,d) ls)
      (== `(,a . ,res) o)
      (appendo d s res))))

(run! 1 q
  (appendo '(a b c) '(d e) q))

(run! 2 q
  (appendo '(a b c) '(d e) q))

(run*! q
  (appendo '(a b c) '(d e) q))

(run! 1 q
  (appendo '(a b c) q '(a b c d e)))

(run! 2 q
  (appendo '(a b c) q '(a b c d e)))

(run! 2 (q r)
  (appendo q r '(a b c d e)))

(run! 7 (q r)
  (appendo q r '(a b c d e)))

(run! 7 (q r s)
  (appendo q r `(a b c d . ,s)))

(run! 8 (q r)
  (appendo r q '(a b c d e)))

(run! 8 (q r)
  (appendo `(a b c ,q) `(d ,r) '(a b c d e)))

; Lets do one more example:
; finding the length of a list.
(define len
  (λ (ls)
    (match ls
      (`() 0)
      (`(,a . ,d) (add1 (len d))))))

(len '(1 2 3 4))

(len '(1 2 3 4 5))

(defrel (add1o n o)
  (pluso '(1) n o))

; Note that in miniKanren we use Oleg numbers and
; zero is represented as '().
(defrel (leno ls o)
  (disj
    (conj (== `() ls) (== '() o))
    (fresh (a d res)
      (== `(,a . ,d) ls)
      (leno d res)
      (add1o res o))))

(run! 1 q (leno '() q))

(run! 1 q (leno '(a b c d e) q))

(run! 1 q
  (fresh (a b c d)
    (leno `(,a ,b ,c ,d) q)))
