#lang pie

#|
Using typed racket we can set the type of append as follows:

(: append (All (A B)
            (-> (Listof A) (Listof B) (Listof (U A B)))))
(define append
  (λ (l1 l2)
    (cond
      ((null? l1) l2)
      (else (cons (car l1) (append (cdr l1) l2))))))

However, we can't make any comments about the length of the
resultant list in the type annotation.
|#

;; Atoms : constructor is '<name>, followed by letters or
;; hyphens, no numbers.

;; Atom
'a
'a--
'abc
'---
; '_
; '-h0t-dog
; ' itself does not make an Atom
;'
'α

;; Nat : constructors are add1 and zero
;; we can use numbers directly as Nat
0
1
zero
(add1 zero)
;; what's another way of writing 3?
3
(add1 (add1 (add1 zero)))

;; Pair : constructor is cons
(the (Pair Nat Nat) (cons 1 2))
(the (Pair Nat Atom) (cons 13 'abc))
(the (Pair Nat (Pair Nat Atom))
  (cons 1 (cons 13 'abc)))

;; -> (function) : constructor is λ
(the (-> Nat
       Nat)
  (λ (x)
    x))

(the (-> Atom
       Atom)
  (λ (x)
    x))

;; What's the type of a type?
;; Every type is a U, except U.
Atom
; (the Atom 0)
(the U Nat)
(Pair Nat Atom)
(the (Pair U U) (cons Atom Nat))
;; Here both U and (Pair U U) have no type, but in a system
;; with a universe hierarchy, both U and (Pair U U) have the
;; type U₁.
(Pair U U)
U

;; There are a fixed number of constructors, we cannot add
;; anymore.
;;; Judgments
;;; 2 is an Atom
;;; ___ is a ___

;;; 2 and 1 are the same Nat
;;; ___ is the same ___ as ___

;;; (add1 2) and 1 are the same Nat
;;; (add1 2) and 3 are the same Nat

;;; Nat is a type
;;; ___ is a type


;;; Nat and Atom are the same type
;;; ___ and ___ are the same type

#|
Four forms of judgments:
1) ____ is a ____
eg:
  'dog is an Atom
  6 is an Atom ;; unlikely to be correct
  (Pair Nat Nat) is a U

2) ____ is the same ____ as ____ (v1 is the same t as v2)
eg:
  0 is the same Nat as zero
  (add1 zero) is the same Nat as 1
  'one is the same Nat as 1 ;; unlikely to be correct

3) ____ is a type (if it's formed by a type constructor)
eg:
  (Pair Nat Nat) is a type
  (Pair Nat Atom) is a type
  Nat is a type
  'bread is a type ;; that's unbelievable!

4) ____ and ____ are the same type
   (t1 and t2 are the same type)
eg:
  Atom and Atom are the same type
  (car (the (Pair U U) (cons Atom Nat))) and Atom are the
  same type
  ;; that's pretty unbelievable!
  Nat and Atom are the same type
  ;; incorrect, because U does not have a type in our system
  U and U are the same type
|#

;; We need to claim before we define
;; claim says something has some type
;; define says something with that type can be defined
(claim four Nat)
(define four 3)

(claim five U)
(define five Nat)

;; We can't reclaim a name that has already been claimed
;; We can't redefine a name that has already been defined
;(define four 4)
;(claim four Atom)

;; An expression with a constructor at the top is a value.
(add1
 (add1 4))

zero
'atom
(the (-> Atom Atom) (λ (x) x))

;; Expressions that are not values and cannot yet be
;; evaluated due to a variable are called neutral.
#;
(cons y 'rutabaga)

;; Suppose we've defined +, this is not a value, because +
;; is not a constructor:
;; (+ (add1 zero) zero)
;; This becomes a value after we place add1 at top:
;; (add1 (+ zero zero))

;; This is a value as well:
;; (add1 (+ (add1 zero) zero))

;; Constructors build values, eliminators take them apart.
;; Eliminators for Nats are:
;;   which-Nat, iter-Nat, and rec-Nat (more will be covered
;;   later)
;; all of them take three arguments: a target, a base and a
;; step.

;; The eliminator for functions is the application to its
;; arguments, i.e., β reduction (a.k.a. symbolic function
;; application defined in the initial law of application on
;; page 38).

;; (which-Nat target base step) is a X if
;;   target is a Nat,
;;   base is an X,
;;   step is a (-> Nat X).
;; Refer to Commandments of which-Nat on page 48 of The
;; Little Typer to know how which-Nat is evaluated.

;; (which-Nat 0 base step) -> base
;; (which-Nat 3 base step) -> (step (sub1 3))

;; We choose the atoms 'true and 'false to act as booleans.
;; target : Nat
;; base : X
;; step: -> Nat X
(which-Nat 0
  'true
  (λ (sub1n)
    'false))

(which-Nat 3
  'true
  (λ (sub1n)
    'false))
;; ->
;; ((λ (sub1n) 'false) 2)
;; ->
;; 'false

(claim step-zero?
  (-> Nat
    Atom))
(define step-zero?
  (λ (n-sub1)
    'false))

(claim zero?
  (-> Nat
      Atom))
(define zero?
  (λ (n)
    (which-Nat n 'true step-zero?)))

(zero? 99)
;; β reduction =>
(which-Nat 99 'true step-zero?)
;; By second commandment of which-Nat (page 48) =>
(step-zero? 98)
;; β reduction =>
'false

;; Let's try defining plus(+)
(claim +
  (-> Nat Nat
    Nat))
#;
(define +
  (λ (m n)
    (which-Nat m n
               (λ (m-1)
                 (add1 (+ m-1 n))))))

;; We can't use recursion in dependent types because
;; recursion could lead to an infinite loop and
;; non-terminating expressions cannot be assigned a type,
;; unlike the Hindley-Milner type inferencer we have
;; implemented. Also pie requires all functions to be total,
;; and non-terminating functions aren't total.

#;
(define +
  (λ (m n)
    (cond
      ((zero? m) n)
      (else
       (let ((res-sub1 (+ (sub1 m) n)))
         (add1 res-sub1))))))

;; (iter-nat target base step) is an X if
;;   target is a Nat,
;;   base is an X,
;;   step is an (-> X X) (which is different from the step
;;   in which-Nat, instead of (-> Nat X), it's (-> X X), as
;;   shown on page 73)
(claim step-+
  (-> Nat
    Nat))
(define step-+
  (λ (r-m-1+n)
    (add1 r-m-1+n)))

(define +
  (λ (m n)
    (iter-Nat m n
      step-+
      ;; this function is extracted out with a new name
      #;
      (λ (m-1+n)
        (add1 m-1+n)))))

(+ 2 5)
;; β reduction =>
(iter-Nat 2 5 step-+)
;; By second commandment of iter-Nat (page 73) =>
(step-+ (iter-Nat 1 5 step-+))
;; β reduction =>
(add1 (iter-Nat 1 5 step-+))
;; By second commandment of iter-Nat (page 73) =>
(add1 (step-+ (iter-Nat 0 5 step-+)))
;; β reduction =>
(add1 (add1 (iter-Nat 0 5 step-+)))
;; By first commandment of iter-Nat (page 73) =>
(add1 (add1 5))
;; equivalent the normal form =>
7

#;
(define gauss
  (λ (n)
    (cond
      ((zero? n) 0)
      (else
       (let ((n-sub1 (sub1 n))
             (res-sub1 (gauss (sub1 n))))
         (+ (add1 n-sub1) res-sub1))))))


;; (rec-nat target base step) is an X if
;;   target is a Nat,
;;   base is an X,
;;   step is an (-> Nat X
;;                X)
(claim step-gauss
  (-> Nat Nat
    Nat))
(define step-gauss
  (λ (n-sub1 res-sub1)
    (+ (add1 n-sub1) res-sub1)))

(claim gauss
  (-> Nat
    Nat))

;; This won't work! Because n inside the inner lambda is a
;; constant by the time we pass a value to gauss.
#;
(define gauss
  (λ (n)
    (iter-Nat n 0
      (λ (gauss-n-1)
        (+ n gauss-n-1)))))

(define gauss
  (λ (n)
    (rec-Nat n 0
      step-gauss
      ;; this is extracted out with a new name
      #;
      (λ (n-1 gauss-n-1)
        (+ (add1 n-1) gauss-n-1)))))

(gauss 3)
;; β reduction =>
(rec-Nat 3 0 step-gauss)
;; By second commandment of rec-Nat =>
(step-gauss 2 (rec-Nat 2 0 step-gauss))
;; β reduction =>
(+ (add1 2) (rec-Nat 2 0 step-gauss))
;; by a series of β reductions and iter-Nat laws as
;; demonstrated in the evaluation example using (+ 2 5) =>
(add1 (add1 (add1
             (rec-Nat 2 0 step-gauss))))

;; By second commandment of rec-Nat =>
(add1 (add1 (add1
             (step-gauss 1 (rec-Nat 1 0 step-gauss)))))
;; β reduction =>
(add1 (add1 (add1
             (+ (add1 1) (rec-Nat 1 0 step-gauss)))))
;; by evaluation of + =>
(add1 (add1 (add1
             (add1 (add1
                    (rec-Nat 1 0 step-gauss))))))
;; By second commandment of rec-Nat =>
(add1 (add1 (add1
             (add1 (add1
                    (step-gauss 0
                                (rec-Nat 0 0 step-gauss)))))))

;; β reduction =>
(add1 (add1 (add1
             (add1 (add1
                    (+ (add1 0)
                       (rec-Nat 0 0 step-gauss)))))))
;; by evaluation of + =>
(add1 (add1 (add1
             (add1 (add1
                    (add1 (rec-Nat 0 0 step-gauss)))))))
;; By first commandment of rec-Nat =>
(add1 (add1 (add1
             (add1 (add1
                    (add1 0))))))
;; equivalent the normal form =>
6

(gauss 5)
(gauss 100)

;; rec-Nat can do anything that which-Nat and iter-Nat can
;; do.
(claim step-zerop
  (-> Nat Atom
    Atom))
(define step-zerop
  (λ (a-1 zerop-sub1)
    'false))

(claim zerop
  (-> Nat
    Atom))
(define zerop
  (λ (a)
    (rec-Nat a 'true step-zerop)))

(zerop 0)
;; β reduction =>
(rec-Nat 0 'true step-zerop)
;; By first commandment of rec-Nat =>
'true

(zerop 100)
;; β reduction =>
(rec-Nat 100 'true step-zerop)

;; By second commandment of rec-Nat =>
(step-zerop 99
  (rec-Nat 99 'true step-zerop))

((the (-> Nat Atom
        Atom)
   (λ (a-1 zerop-sub1)
     'false))
 99
 (rec-Nat 99 'true step-zerop))

;; β reduction =>
'false
