#lang racket
(require "mk.rkt")
(require "numbers.rkt")


;;Part 1
(run 2 (q)
  (conj
   (== 5 q)
   (disj
    (conj
     (disj
      (conj
       (== 5 q)
       (== 6 q)))
     (== 5 q))
    (== q 5))))

;(cons '(((5) () () ())) (Relay (var 'q #<Scope> '#() (set)) '()))
;Mini kanren arrives at the answer of (5) for this problem. The (5) in the nested lists is what q has to be equal to for this function that takes a substitution to succeed
;The 3 empty lists after it are the constraints, which are blank in this case because the only constraint is that q must equal 5. The constraints
;usually show what the specific variables that get returned have to not be, like the next example. After the constraints, the Relay part of the ouput
;is how minikanren handles its infinite list style of output, to just grab that one 5.

(run 1 (q)
  (fresh (a b)
     (conj
      (== `(,a ,b) q)
      (absento 'tag q)
      (symbolo a))))
;(cons '((((_0 _1)) ((symbolo _0)) ((=/= _1 tag) (=/= _0 tag)) ((absento _0 (tag)) (absento _1 (tag))))) (Relay (var 'q #<Scope> '#() (set)) '()))
;This function that takes in a substitution returns (_0 _1) as its output, meaning that the inputs can be anything, which we call reified variables.
;Then, unlike the last example, these reified variables have constraints after. In this return, _0 must be a symbol, and both _1 and _0 must not be tags.
;Then _0 must not be in the (tag), and same with _1. Then the last part is just the standard expressing of how minikanren actually handles the results it generates
;with the infinite set, and then picks the simplest sequence, which in this case is the only one.

;==. This is the equals sign, which is just checking equivalence. It could be used with two variables, a fresh variable and a given variable, two numbers
;, and basically anything you would use equals? or eqv? in normal Racket. 

;=/=. This it the not equals sign, which is checking for non-equivalence. It can once again be used with two variables, a fresh variable and a given variable, two numbers
;, and basically anything you would do (not (eqv? ... ...)) in racket, I don't remember if we've used an actual not equals sign.

;numbero ;Numbero is basically the minikanren version of number?, which is just checking if whatever you give it is a number or not. This can be used within the body of
;Minikanren functions to have a conditon to check if a variable, or a hardcoded value is a number. Will pass if the given is a number, and fail if not.

;symbolo. Symbolo is basically the minikanren version of symbol?, which is just checking if whatever you give it is a symbol or not. This can be used within the body of
;Minikanren functions to have a conditon to check if a variable, or a hardcoded value is truly a symbol or not? Will pass if the given is a symbol, and fail if not.

;absento. This is a function kinda like assv in normal Racket, but the opposite. Absento ensures that a tag you give it does not appear in the term you give it,
;which when calling it is like (absento tag term). If the tag is absent, passes. If the tag is present, fails.


;Part 2
(defrel (assoco x ls o)
  (fresh (a d aa da)
         (conj
          (== `(,a . ,d) ls)
          (== `(,aa . ,da) a))
         (disj
          (conj
           (== aa x)
           (== a o))
          (conj
           (=/= aa x)
           (assoco x d o)))))

(defrel (reverseo ls o)
  (disj
   (conj
    (== '() ls)
    (== o '()))
  (fresh (a d res)
          (== `(,a . ,d) ls)
          (reverseo d res)
          (appendo res `(,a) o))))


(defrel (stuttero ls o)
  (disj
   (conj
    (== '() ls)
    (== o '()))
   (fresh (a d res)
          (== `(,a . ,d) ls)
          (== `(,a ,a . ,res) o)
          (stuttero d res))))

(defrel (add1o n o)
  (pluso '(1) n o))
(defrel (lengtho ls o)
  (disj
    (conj (== `() ls) (== '() o))
    (fresh (a d res)
      (== `(,a . ,d) ls)
      (lengtho d res)
      (add1o res o))))
;;(run! 1 q (lengtho '(a b c d e) q))