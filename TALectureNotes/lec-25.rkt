#lang pie

(claim append
  (Π ((E U))
    (-> (List E)
        (List E)
      (List E))))

;; Original definition follows property 1 of append
(define append
  (λ (E)
    (λ (start end)
      (rec-List start end
        (λ (car-start cdr-start append-cdr-start)
          (:: car-start append-cdr-start))))))

;; The following definition follows property 1 but doesn't
;; follow property 2
#;
(define append
  (λ (E)
    (λ (start end)
      (rec-List start end
        (λ (car-start cdr-start append-cdr-start)
          (:: car-start end))))))

(append Atom (:: 'a (:: 'b nil)) (:: 'c (:: 'd nil)))

;; Properties of append:
;; - The type of output sequence is the same as type of
;;   both input sequences;
;; - Sum of the length of the output sequence equals
;;   the sum of the lengths of the input sequences;
;; - A member belongs to the output sequence if and only if
;;   that member belongs to either of the input sequences.

;; The Vector type is like a List type that also contains
;; its length.
;; Vectors have two constructors: vec:: and vecnil.

(the (Vec Atom 2) (vec:: 'a (vec:: 'b vecnil)))

(claim rugbrod-vec
  (Vec Atom 5))
(define rugbrod-vec
  (vec:: 'rye-flour
    (vec:: 'rye-kernel
      (vec:: 'water
        (vec:: 'sourdough
          (vec:: 'salt vecnil))))))

(claim toppings-vec
       (Vec Atom 2))
(define toppings-vec
  (vec:: 'potato
    (vec:: 'butter vecnil)))


(the (Vec Atom 5) rugbrod-vec)
(the (Vec Atom 6) (vec:: 'pumpkin-seeds rugbrod-vec))
;; (the (Vec Atom 6) (vec:: 'pumpkin-seeds TODO))

(the (Vec Atom 0) vecnil)
(the (Vec Nat 0) vecnil)
(the (Vec (List Atom) 0) vecnil)
(the (Vec (car (the (Pair U U) (cons Nat Atom))) 0) vecnil)

;; 2 eliminators for Vec are: head and tail.
(claim first-1
  (Π ((E U))
    (-> (Vec E 1)
      E)))
(define first-1
  (λ (E)
    (λ (es)
      (head es))))

(first-1 Nat (vec:: 5 vecnil))

(claim first-2
  (Π ((E U))
    (-> (Vec E 2)
      E)))
(define first-2
  (λ (E)
    (λ (es)
      (head es))))

(first-2 Nat (vec:: 5 (vec:: 6 vecnil)))
#;
(first-2 Atom (vec:: 'matsutake
                (vec:: 'morel
                  (vec:: 'truffle vecnil))))

;; This definition is different from the one in
;; The Little Typer because it has swapped the
;; two arguments l and E for simplicity and for
;; demonstrating that it is fine to do so because
;; they're not dependent on each other.
(claim first
  (Π ((l Nat)
      (E U))
    (-> (Vec E (add1 l)) ;; this is the first time we're
                         ;; doing arithmetic in the type
                         ;; definition
      E)))
(define first
  (λ (l E)
    (λ (es)
      (head es))))

(first 1 Nat (vec:: 5 (vec:: 6 vecnil)))
(first 0 Nat (vec:: 23 vecnil))
(first 1 Nat (vec:: 23 (vec:: 24 vecnil)))
(first 2 Nat (vec:: 23 (vec:: 24 (vec:: 25 vecnil))))
(first 3 Atom
  (vec:: 'a (vec:: 'b (vec:: 'c (vec:: 'd vecnil)))))

;; We can use first to define first-3
(claim first-3
  (Π ((E U))
    (-> (Vec E 3)
      E)))
(define first-3
  (first 2))

(first-3 Atom
         (vec:: 'matsutake
           (vec:: 'morel
             (vec:: 'truffle vecnil))))

(claim rest
  (Π ((l Nat)
      (E U))
    (-> (Vec E (add1 l))
      (Vec E l))))
(define rest
  (λ (l E)
    (λ (es)
      (tail es))))

(rest 0 Nat (vec:: 23 vecnil))
(rest 1 Nat (vec:: 23 (vec:: 24 vecnil)))
(rest 2 Nat (vec:: 23 (vec:: 24 (vec:: 25 vecnil))))

;; Let's write a function that gives
;; us a vector of 'pea based on input Nat

;; This won't work because the base and the step are
;; not returning the same type
#;
(define peas
  (λ (n)
    (rec-Nat n (the (Vec Atom zero) vecnil)
      (λ (sub1-n peas-sub-n)
        (vec:: 'pea peas-sub-n)))))

;; (ind-Nat target motive base step) is a (motive target)
;; if
;;   target is a Nat,
;;   motive is a (-> Nat U),
;;   base is a (motive zero)
;; and
;;   step is a
;;     (Π ((sub1-n Nat))
;;       (-> (motive sub1-n)
;;         (motive (add1 sub1-n))))
;; see page 147.
(claim peas
  (Π ((how-many-peas Nat))
    (Vec Atom how-many-peas)))

(define peas
  (λ (how-many-peas)
    (ind-Nat how-many-peas
      (λ (n)
        (Vec Atom n))
      vecnil
      (λ (sub1-n peas-sub1-n)
        (vec:: 'pea peas-sub1-n)))))

(peas 7)

;; Similar to ind-Nat we also have ind-Vec

;; (ind-Vec n target motive base step) is a
;; (motive n target) if
;;   target is a (Vec E n),
;;   motive is a
;;     (Π ((k Nat))
;;       (-> (Vec E k)
;;         U),
;;   base is a (motive zero vecnil)
;; and
;;   step is a
;;     (Π ((k Nat)
;;         (h E)
;;         (t (Vec E k)))
;;       (-> (motive k t)
;;         (motive (add1 k) (vec:: h t))))
;; see page 248.

(claim +
  (-> Nat Nat
    Nat))
(define +
  (λ (a b)
    (iter-Nat a b (λ (res-a-sub1)
                    (add1 res-a-sub1)))))

(claim step-append-vec
  (Π ((E U)
      (n Nat)
      (k Nat)
      (hd E)
      (tl (Vec E k)))
    (-> (Vec E (+ k n))
      (Vec E (+ (add1 k) n)))))
(define step-append-vec
  (λ (E)
    (λ (n k)
      (λ (hd tl)
        (λ (append-tl-es-end)
          (vec:: hd append-tl-es-end))))))

(claim append-vec
  (Π ((E U)
      (m Nat)
      (n Nat))
    (-> (Vec E m)
        (Vec E n)
      (Vec E (+ m n)))))
(define append-vec
  (λ (E)
    (λ (m n)
      (λ (es end)
        (ind-Vec m es
          (λ (k)
            (λ (v)
              (Vec E (+ k n))))
          end
          (step-append-vec E n))))))

(append-vec
  Atom 2 3
  (vec:: 'a (vec:: 'b vecnil))
  (vec:: 'y (vec:: 'w (vec:: 'v vecnil))))

(append-vec Nat 3 4
  (vec:: 5 (vec:: 1 (vec:: 3 vecnil)))
  (vec:: 2 (vec:: 9 (vec:: 8 (vec:: 6 vecnil)))))

;; This is in normal form
;; (λ (b)
;;   (add1 b))
;; because it has λ at the top and since λ is a
;; constructor, it is a value.
(+ 1)

;; If there's a neutral/free variable inside a λ then it's
;; not in normal form, it's a neutral expression.
;; But, some neutral expressions can be turned into a value.
(claim incr
  (-> Nat
    Nat))
(define incr
  (λ (n)
    (iter-Nat n
      1
      (+ 1))))

;; This also reduces to a normal form but
;; it's not the same (-> Nat Nat) as (+ 1)
incr

;; But, incr and (+ 1) do produce the same results
;; when applied with the same arguments

((+ 1) 2)
(incr 2)

((+ 1) 4999)
(incr 4999)

;; So, we need a new form to express this sameness.
;; Let us express this sameness using a new type = or
;; equality
(= Nat 2 (+ 1 1))
(= (Pair Nat Nat) (cons 2 (+ 1 1)) (cons (+ 1 1) 2))
(= (car (the (Pair U U) (cons Nat Atom))) 42 42)

;; (= X from to) is a type provided that X is a type, and
;; from and to inhabit the type X.

;; Neither of the following are a type
;; (= Nat 2 'five)
;; (= (cons Nat Atom) (cons 10 'ten) (cons 20 'fifty))

;; This expression is like the judgment "2 is the same Nat
;; as 5". Similar to how the above judgment is still a
;; judgment even if it's false, the type below is also a
;; type even if the from and to are not the same Nat.
(= Nat 2 5)

;; Similar to how = types can be read as a statement which
;; states that two values are the same, there are other
;; types which can be interpreted as statements, too.

;; Π types can be interpreted as "for all".
;; The type below can be stated as:
;; "For all Natural numbers n, 1 + n equals n + 1"
(Π ((n Nat))
  (= Nat (+ 1 n) (+ n 1)))

;; -> types can be interpreted as "if ... then ..."
;; The type below can be stated as:
;; "For all natural numbers n and functions from Naturals to
;; Naturals f & g, if f applied to n equals g applied to n,
;; then f equals g "
(Π ((n Nat)
    (f (-> Nat
         Nat))
    (g (-> Nat
         Nat)))
  (-> (= Nat (f n) (g n))
    (= (-> Nat Nat) f g)))

;; Not all statements derived from types are interesting.

;; For the type/statement/theorem Nat what is the proof?

;; If we want to prove a statement that is represented by a
;; type, then the proof is a value that inhabits the type.

;; Nat is inhabited by any natural number.
(claim proofNat
  Nat)
;; 15 is a Nat so it is a proof of Nat
(define proofNat
  (the Nat 15))

;; The following type states that if we have an Atom then
;; we can get a Nat.
(claim proof-Atom->Nat
  (-> Atom Nat))

;; The proof of the above statement is:
(define proof-Atom->Nat
  (λ (a) zero))

;; How do we prove the equality type? We use the "same"
;; constructor.

;; (same e) inhabits the type (= X e e) if e is an X.

(the (= Nat 2 (+ 1 1)) (same 2))
(the (= (Pair Nat Nat) (cons 2 (+ 1 1)) (cons (+ 1 1) 2))
  (same (cons 2 2)))
(the (= (car (the (Pair U U) (cons Nat Atom))) 42 42)
  (same (+ 22 20)))

;; The following fails
#;
(the (= (car (the (Pair U U) (cons Nat Atom))) 32 42)
  (same (+ 22 10)))

;; Let's try to prove something interesting
(claim +1=add1
  (Π ((n Nat))
    (= Nat ((+ 1) n) (add1 n))))
(define +1=add1
  (λ (n)
    ;; We can use TODO here to get the normal form of the
    ;; type of +1=add1
    (same (add1 n))))

;; Type of step according to definition of ind-Nat
#;
(claim step-incr=+1
  (Π ((n Nat))
    (-> (motive n)
      (motive (add1 n)))))
;; Using the motive function in ind-Nat of incr=add1
#;
(claim step-incr=+1
  (Π ((n Nat))
    (-> ((λ (k)
           (= Nat (incr k) ((+ 1) k)))
          n)
      ((λ (k)
         (= Nat (incr k) ((+ 1) k)))
        (add1 n)))))
;; β reduction
#;
(claim step-incr=+1
  (Π ((n Nat))
    (-> (= Nat (incr n) ((+ 1) n))
      (= Nat (incr (add1 n)) ((+ 1) (add1 n))))))
;; β reduction
#;
(claim step-incr=+1
  (Π ((n Nat))
    (-> (= Nat (incr n) (add1 n))
      (= Nat (incr (add1 n)) (add1 (add1 n))))))

;; The above type and below type are the same because
;; (incr (add1 n)) and (add1 (incr n)) have the same normal
;; forms. You can confirm this by using = and same.
(claim step-incr=+1
  (Π ((n Nat))
    (-> (= Nat (incr n) (add1 n))
      (= Nat (add1 (incr n)) (add1 (add1 n))))))
(define step-incr=+1
  (λ (n)
    (λ (incr=+1-sub1-n)
      ;; We can use TODO to get the type
      #;
      TODO
      (cong incr=+1-sub1-n (+ 1)))))

;; cong is an eliminator for =

;; (cong target f) is a (= Y (f from) (f to)) provided that
;; target is a (= X from to) and f is a (-> X Y)

;;(incr (add1 n)) is the same as (add1 (incr n))
(claim incr-add1=add1-incr
  (Π ((n Nat))
    (= Nat (incr (add1 n)) (add1 (incr n)))))
(define incr-add1=add1-incr
  (λ (n)
    (same (incr (add1 n)))))

(claim incr=+1
  (Π ((n Nat))
    (= Nat (incr n) ((+ 1) n))))

;; This is wrong
#;
(define incr=+1
  (λ (n)
    (same (add1 n))))

(define incr=+1
  (λ (n)
    ;; We can use TODO to get the type
    #; TODO
    (ind-Nat n
      (λ (k)
        (= Nat (incr k) ((+ 1) k)))
      ;; base should be (= Nat (incr 0) ((+ 1) 0))
      (same 1)
      step-incr=+1)))

incr=+1

;;; Optional
;; Proof for associativity of +
(claim step-+-assoc
  (Π ((m Nat)
      (n Nat))
    (Π ((k Nat))
       (-> (= Nat (+ k (+ m n)) (+ (+ k m) n))
         #;
         (= Nat (+ (add1 k) (+ m n)) (+ (+ (add1 k) m) n))
         (= Nat (add1 (+ k (+ m n))) (add1 (+ (+ k m) n)))))))
(define step-+-assoc
  (λ (m n)
    (λ (k)
      (λ (proof-k)
        (cong proof-k (+ 1))))))

(claim +-assoc
  (Π ((l Nat)
      (m Nat)
      (n Nat))
    (= Nat (+ l (+ m n)) (+ (+ l m) n))))

(define +-assoc
  (λ (l m n)
    (ind-Nat l
      (λ (k)
        (= Nat (+ k (+ m n)) (+ (+ k m) n)))
      (same (+ 0 (+ m n)))
      (step-+-assoc m n))))

+-assoc

;; Similar to ind-Vec, we also have ind-List.
;; (ind-List target motive base step) is a (motive target)
;; if
;;   target is a (List E),
;;   motive is a (-> (List E)
;;                 U),
;;   base is a (motive nil)
;; and
;;   step is a
;;     (Π ((e E)
;;         (es (List E)))
;;       (-> (motive es)
;;         (motive (:: e es))))
;; see page 237.

;; The first commandment of ind-List
;; The ind-List-expression
;;   (ind-List nil
;;     mot
;;     base
;;     step)
;; is the same (mot nil) as base.

;; The second commandment of ind-List
;; The ind-List-expression
;;   (ind-List (:: e es)
;;     mot
;;     base
;;     step)
;; is the same (mot (:: e es)) as
;;   (step e es
;;     (ind-List es
;;       mot
;;       base
;;       step))
;; see page 238.

;; With ind-List, we can prove some properties of append.
