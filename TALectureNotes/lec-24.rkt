#lang pie

(the (Pair Nat Nat) (cons 1 2))

(claim p
  (Pair Atom Nat))
(define p
  (cons 'dog 23))

;; The eliminators for Pair types are car and cdr.
(car p)
(cdr p)

(claim p^
  (Pair
    (Pair Nat Atom)
    (Pair Atom Nat)))
(define p^
  (cons
    (cons 2 'a)
    (cons 'b 5)))

(cdr (car p^))

#;
(cdar p^)
(car (cdr p^))

(claim Pear U)
(define Pear (Pair Nat Nat))

(claim pe Pear)
(define pe
  (cons 4 5))

(claim Avocado U)
(define Avocado
  (Pair Atom Nat))

(claim pe^ Avocado)
(define pe^
  (cons 'bat 5))

(claim flip-Pear
  (-> Pear
    Pear))
(define flip-Pear
  (λ (p)
    (cons (cdr p) (car p))))

(flip-Pear pe)

(claim flip-Avocado
  (-> Avocado
    (Pair Nat Atom)))
(define flip-Avocado
  (λ (p)
    (cons (cdr p) (car p))))

(flip-Avocado pe^)

#;
(claim flip-pan
  (-> A D (Pair A D)
      (Pair D A)))
#;
(define flip-pan
  (λ (p)
    (cons (cdr p) (car p))))

#;(claim A U)

#;(claim D U)

;; We need Π types to express a polymorphic function where
;; subsequent types are dependent on previous types.

;; When a variable declared in a Π type (like p above) is
;; never used, it can be written as the usual -> type.  Both
;; Π and -> are types of functions constructed using
;; lambda.
(claim flip-pan
  ;; pan as in pancontinental or panPair meaning across all
  ;; Pair types
  (Π ((A U)
       (D U))
    (-> (Pair A D)
      (Pair D A))))
(define flip-pan
  (λ (A D)
    (λ (p)
      (cons (cdr p) (car p)))))

;; Everything is curried automatically so the order in which
;; we curry doesn't matter.

;; The following definitions of flip-pan are the same.
#;
(define flip-pan
  (λ (A D p)
    (cons (cdr p) (car p))))
#;
(define flip-pan
  (λ (A)
    (λ (D)
      (λ (p)
        (cons (cdr p) (car p))))))

;; The spanning of Curryed arguments doesn't matter.
((flip-pan Nat Nat) pe)
((flip-pan Atom Nat) pe^)

((flip-pan Nat) Nat pe)
(flip-pan Nat Nat pe)

(flip-pan Pear Avocado (cons (cons 5 6)
                         pe^))

;; Functions are automatically Curryed but eliminators,
;; constructors, and type constructors can't be Curryed.
;; ((flip-pan Nat) Nat ((cons 2) 3))
;; ((Pair Nat) Nat)
;; ((which-Nat 10 0) (λ (x) x))

;; Let's define our own eliminator for Pair that abstracts
;; away the use of car and cdr.
(claim elim-Pair
  (Π ((A U)        ;; type of car of target pair
      (D U)        ;; type of cdr of target pair
      (P U))       ;; type of the output of elim-Pair
    (-> (Pair A D) ;; target pair
        (-> A D    ;; tranformation function that returns
          P)       ;; something after processing the car/cdr.
      P)))
(define elim-Pair
  (λ (A D P) ;; we don't use types A, D, and P in the body
    (λ (p f)
      (f (car p) (cdr p)))))

;; Use TODO to figure out the type of missing values.
#;
(define swap
  (λ (A D)
    (λ (p)
      (elim-Pair A D (Pair D A) p
        TODO))))

;; swap is just flip-pan but using elim-Pair and not car/cdr.
(claim swap
  (Π ((A U)
       (D U))
    (-> (Pair A D)
      (Pair D A))))
(define swap
  (λ (A D)
    (λ (p)
      (elim-Pair A D (Pair D A) p
                 (λ (a d)
                   (cons d a))))))

;; All Curryed forms are equivalent.
((swap Nat Nat) pe)
((swap Atom Nat) pe^)

((swap Nat) Nat pe)
(swap Nat Nat pe)

;;; The difference between Pair and List is that all members
;;; in a List must have the same type, but the car and cdr
;;; of a Pair can have different types.
(List Pear)
; ::
(the (List Pear) nil)
(the (List Pear) (:: pe nil))
(the (List Pear) (:: pe (:: pe nil)))

;; rec-List is one of the eliminators for List, which takes 3
;; arguments, see the Law of rec-List on page 116.
;; (rec-List target base step) is an X if
;;   target is a (List E),
;;   base is an X,
;;   step is an (-> E (List E) X
;;                X)
#;
(step (first target)
      (rest target)
      (rec-List (rest target) base step))
;; rec-List is one of the eliminators for List, which takes 3
;; arguments, see the Law of rec-List on page 116.
;; (rec-List target base step) is an X if
;;   target is a (List E),
;;   base is an X,
;;   step is an (-> E (List E) X
;;                X)
(claim length
  (Π ((E U))
    (-> (List E)
      Nat)))
(define length
  (λ (E)
    (λ (l)
      (rec-List l 0
                (λ (a d r)
                  (add1 r))))))

(length Pear (the (List Pear) (:: pe (:: pe nil))))
(length Pear (:: pe (:: pe nil)))
(length Pear nil)

(claim +
  (-> Nat Nat
    Nat))
(define +
  (λ (x y)
    (iter-Nat x
      y
      (λ (r_x-1)
        (add1 r_x-1)))))

(claim sum-list
  (-> (List Nat)
    Nat))
(define sum-list
  (λ (l)
    (rec-List l 0
              (λ (a d r)
                (+ a r)))))

(sum-list (:: 1 (:: 2 (:: 3 (:: 4 nil)))))

;; Thanks Haskell and Lisp, :: and nil are here.
(claim append
  (Π ((E U))
    (-> (List E) (List E)
      (List E))))
(define append
  (λ (E)
    (λ (l1 l2)
      (rec-List l1 l2
                (λ (a d r)
                  (:: a r))))))

(claim condiments
  (List Atom))
(define condiments
  (:: 'chives
    (:: 'mayonaisse nil)))

(claim toppings
  (List Atom))
(define toppings
  (:: 'potato
    (:: 'butter nil)))

(append Nat (:: 1 (:: 2 nil)) (:: 3 (:: 4 nil)))
(append Atom condiments toppings)

(claim snoc
  (Π ((E U))
    (-> (List E) E
      (List E))))
(define snoc
  (λ (E)
    (λ (l e)
      (rec-List l (:: e nil)
                (λ (a d r)
                  (:: a r))))))

(claim rugbrod
  (List Atom))
(define rugbrod
  (:: 'rye-flour
    (:: 'rye-kernel
      (:: 'water
        (:: 'sourdough
            (:: 'salt nil))))))

(snoc Atom rugbrod 'pumpkin-seeds)

(claim reverse
  (Π ((E U))
    (-> (List E)
      (List E))))
(define reverse
  (λ (E)
    (λ (l)
      (rec-List l (the (List E) nil)
                (λ (a d r)
                  (snoc E r a))))))

(reverse Atom rugbrod)

(claim kartoffelmad
  (List Atom))
(define kartoffelmad
  (append Atom
    (append Atom condiments toppings)
    (reverse Atom (:: 'plate (:: 'rye-bread nil)))))

;; The following are optional contents.
;;
;; For those who are interested, take a look at the
;; following code for generating trees using Pair.
(claim twin
  (Π ((T U))
    (-> T
      (Pair T T))))
(define twin
  (λ (T)
    (λ (v)
      (cons v v))))

(twin Nat 42)

(claim Twin
  (-> U
    U))
(define Twin
  (λ (T)
    (Pair T T)))

(claim t
  (Twin (Pair Atom Atom)))
(define t
  (twin (Pair Atom Atom) (cons 'a 'b)))

;; The nested pair t can be represented as a tree like this:
;; N
;; |
;; |-----|
;; N     N
;; |     |
;; |--|  |--|
;; 'a 'b 'a 'b

;; The type of the above tree is:
(Pair (Pair Atom Atom)
  (Pair Atom Atom))

;; Similarly, a deeply nested pair tree of depth 3 can be
;; represented as follows:
;; N
;; |-----------|
;; N           N
;; |           |
;; |-----|     |-----|
;; N     N     N     N
;; |     |     |     |
;; |--|  |--|  |--|  |--|
;; 6  3  7  2  9  8  2  5

;; We can consider each node N to be the cons constructor or
;; the Pair type constructor.

;; The type of the above tree is:
(Pair
  (Pair
    (Pair Nat Nat)
    (Pair Nat Nat))
  (Pair
    (Pair Nat Nat)
    (Pair Nat Nat)))

;; When a depth d and the type of every leaf node T is
;; given, Tree will return the type of the tree like the
;; type of the depth 3 tree above.
(claim Tree
  (Π ((d Nat)
      (T U))
    U))
(define Tree
  (λ (d T)
    (iter-Nat d T Twin)))

(Tree 3 Nat)

;; tree is the depth 3 tree depicted in the above diagram.
(claim tree
  (Tree 3 Nat))

(define tree
  (cons
    (cons (cons 6 3)
          (cons 7 2))
    (cons (cons 9 8)
          (cons 2 5))))

(claim tree-clones
   (Tree 3 Nat))
(define tree-clones
  (cons
   (cons (cons 4 4)
         (cons 4 4))
   (cons (cons 4 4)
         (cons 4 4))))
;; We can write a function like twin that takes the depth d
;; of the tree and a value v to be cloned, then it returns a
;; nested pair tree of depth d with values v at the leaf
;; node.

;; Using the above described function we can generate the
;; value of tree-clones without manually typing the entire
;; cons structure. However, to do this we will need the
;; ind-Nat eliminator which is covered in chapter 7 of
;; The Little Typer.
