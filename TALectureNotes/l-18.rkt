#lang racket
(require "mk-new.rkt")

;;; the environment
(define (apply-env env x)
  (match env
    (`() (error 'apply-env "Not found: " x))
    (`((,var . ,val) . ,rest-env)
     (cond
       ((eqv? var x) val)
       (else (apply-env rest-env x))))))

(define (empty-env) `())

(define (extend-env var val env)
  `((,var . ,val) . ,env))

(define (not-in-env x env)
  (match env
    ('() #t)
    (`((,var . ,val) . ,env^)
     (cond
       ((eqv? var x) #f)
       (else (not-in-env x env^))))))

;;; the interpreter
(define (valof expr env)
  (match expr
    (`,n #:when (number? n) n)
    (`,b #:when (boolean? b) b)
    (`,s #:when (symbol? s) (apply-env env s))
    (`(* ,e1 ,e2)
     #:when (not-in-env '* env)
     (* (valof e1 env) (valof e2 env)))
    (`(zero? ,e1)
     #:when (not-in-env 'zero? env)
     (zero? (valof e1 env)))
    (`(sub1 ,e1)
     #:when (not-in-env 'sub1 env)
     (sub1 (valof e1 env)))

    (`(if ,test ,conseq ,alt)
     #:when (not-in-env 'if env)
     (if (valof test env)
         (valof conseq env)
         (valof alt env)))
    (`(fix ,f) #:when (not-in-env 'fix env)
     (letrec ((res
               ; without this inverse-η,
               ; it will go into an infinite loop
               (λ (arg)
                 (((valof f env) res) arg))))
       res))
    (`(λ (,x) ,body)
     #:when (not-in-env 'λ env)
     (λ (arg)
       (valof body (extend-env x arg env))))
    (`(,rator ,rand)
     ((valof rator env) (valof rand env)))))

;;; the type inferencer
;; x has type τ in Γ
;; Input ∈:
;; for Window: type '\in', then 'Alt + \'
;; for Mac: type '\in', then 'Control + \'
;; Input τ:
;; for Window: type '\tau', then 'Alt + \'
;; for Mac: type '\tau', then 'Control + \'
(defrel (∈ x τ Γ)
  (fresh (xa τa restΓ)
    (conj
      (== `((,xa . ,τa) . ,restΓ) Γ)
      (disj
        (conj
          (== x xa)
          (== τa τ))
        (conj
          (=/= x xa)
          (∈ x τ restΓ))))))

(defrel (not-in-Γo x Γ)
  (disj
    (conj (== '() Γ) succeed)
    (fresh (y τy resΓ)
      (conj
        (== `((,y . ,τy) . ,resΓ) Γ)
        (disj
          (conj (== y x) fail)
          (conj
            (=/= y x)
            (not-in-Γo x resΓ)))))))


;; the environment
(define (apply-env env x)
  (match env
    [`() (error 'apply-env "Not found: " x)]
    [`((,var . ,val) . ,rest-env)
     (cond
       [(eqv? var x) val]
       [else (apply-env rest-env x)])]))

(define (empty-env) `())

(define (extend-env var val env)
  `((,var . ,val) . ,env))

(define (not-in-env x env)
  (match env
    ['() #t]
    [`((,var . ,val) . ,env^)
     (cond
       [(eqv? var x) #f]
       [else (not-in-env x env^)])]))

;; the interpreter
(define (valof expr env)
  (match expr
    [`,n #:when (number? n) n]
    [`,b #:when (boolean? b) b]
    [`,s #:when (symbol? s) (apply-env env s)]
    [`(* ,e1 ,e2)
     #:when (not-in-env '* env)
     (* (valof e1 env) (valof e2 env))]
    [`(+ ,e1 ,e2)
     #:when (not-in-env '+ env)
     (+ (valof e1 env) (valof e2 env))]
    [`(zero? ,e1)
     #:when (not-in-env 'zero? env)
     (zero? (valof e1 env))]
    [`(sub1 ,e1)
     #:when (not-in-env 'sub1 env)
     (sub1 (valof e1 env))]
    [`(if ,test ,conseq ,alt)
     #:when (not-in-env 'if env)
     (if (valof test env)
         (valof conseq env)
         (valof alt env))]
    [`(let (,y ,v) ,b)
     #:when (and (symbol? y)
                 (not-in-env 'let env))
     (valof b (extend-env y (valof v env) env))]
    [`(fix ,f)
     #:when (not-in-env 'fix env)
     (letrec ((res
               ; without this inverse-η,
               ; it will go into an infinite loop
               (λ (arg)
                 (((valof f env) res) arg))))
       res)]
    [`(λ (,x) ,body)
     #:when (not-in-env 'λ env)
     (λ (arg)
       (valof body (extend-env x arg env)))]
    [`(,rator ,rand)
     ((valof rator env) (valof rand env))]))

;; This gives us an error because "*" operator expects two operands of "type"
;; number, but the first operand is of the "type" boolean. We call these kind of
;; errors as type errors.
#;
(valof '(* (zero? 5) 5) (empty-env))


;; Instead of coming across these type errors _while_ running the program, we
;; can detect type errors _before_ we run the program. We can do that using a
;; type system that helps us check the type of a program before we run it. The
;; type system that we use is called the Hindley-Milner type system and we will
;; be implementing it using miniKanren.

#|

A judgement J looks like

Γ ⊢ e : τ

where Γ is a typing environment,
e is an expression, and τ is a type

types can be
Bool
Nat


An inference rule looks like

J₀ J₁ ... (premises)
--------------
J (conclusion)

Here are the inference rules for our type system that we will use to define the
typing judgement relation.

(x : τ) ∈ Γ
------------ var
Γ ⊢ x : τ


Γ ⊢ e1 : Nat
Γ ⊢ e2 : Nat
---------------------   *
Γ ⊢ (* e1 e2) : Nat

Γ ⊢ e1 : Nat
Γ ⊢ e2 : Nat
----------------------  +
Γ ⊢ (+ e1 e2) : Nat


Γ ⊢ e : Nat
----------------------   zero?
Γ ⊢ (zero? e) : Bool


Γ ⊢ e : Nat
----------------------   sub1
Γ ⊢ (sub1 e) : Nat


Γ ⊢ test : Bool
Γ ⊢ conseq : τ
Γ ⊢ alt : τ
----------------------------   if
Γ ⊢ (if test conseq alt) : τ



Γ,(x : τx) ⊢ body : τbody
----------------------------   λ
Γ ⊢ (λ (x) body) : (-> τx τbody)



Γ ⊢ rator : (-> τx τ)
Γ ⊢ rand : τx
----------------------------   app
Γ ⊢ (rator rand) : τ


Γ ⊢ f : ((τ^ → τ^^) → (τ^ → τ^^))
-----------------------------------   fix
Γ ⊢ (fix f) : τ^ → τ^^

|#

;; Here's how to type ∈:
;; for Windows: type '\in', then 'Alt + \'
;; for MacOS: type '\in', then 'Control + \'

;; Relation that says that the symbol x is associated with the type τ within the
;; context Γ
(defrel (∈ Γ x τ)
  (fresh (x^ τ^ Γ^)
         (conj (== Γ `((,x^ . ,τ^) . ,Γ^))
               (disj (conj (== x x^)
                           (== τ τ^))
                     (conj (=/= x x^)
                           (∈ Γ^ x τ))))))

;; Here's how to type ⊢:
;; for Windows: type '\vdash', then 'Alt + \'
;; for MacOS: type '\vdash', then 'Control + \'

;; Relation that says that expression e has type τ in context Γ
(defrel (⊢ Γ e τ)
  (disj
   (conj (numbero e)
         (== τ 'Nat))
   (conj (disj (== e #f)
               (== e #t))
         (== τ 'Bool))
   (conj (symbolo e)
         (∈ Γ e τ))
   (fresh (e1 e2)
          (conj (== `(* ,e1 ,e2) e)
                (== 'Nat τ)
                (⊢ Γ e1 'Nat)
                (⊢ Γ e2 'Nat)))
   (fresh (test conseq alt)
          (conj (== `(if ,test ,conseq ,alt) e)
                (⊢ Γ test 'Bool)
                (⊢ Γ conseq τ)
                (⊢ Γ alt τ)))
   (fresh (e1)
          (conj (== `(zero? ,e1) e)
                (== 'Bool τ)
                (⊢ Γ e1 'Nat)))
   (fresh (e1)
          (conj (== `(sub1 ,e1) e)
                (== 'Nat τ)
                (⊢ Γ e1 'Nat)))
   (fresh (x body τx τbody)
          (conj (symbolo x)
                (== `(λ (,x) ,body) e)
                (== `(-> ,τx ,τbody) τ)
                (⊢ `((,x . ,τx) . ,Γ) body τbody)))
   (fresh (rator rand τx)
          (conj (== `(,rator ,rand) e)
                (⊢ Γ rator `(-> ,τx ,τ))
                (⊢ Γ rand τx)))
   (fresh (τ^ τ^^ f)
          (conj (== `(fix ,f) e)
                (== `(-> ,τ^ ,τ^^) τ)
                (⊢ Γ f `(-> (-> ,τ^ ,τ^^) (-> ,τ^ ,τ^^)))))))

(run! 2 q (⊢ '() 5 q))
(run! 2 q (⊢ '() #t q))
(run! 2 q (⊢ '() #f q))
(run! 2 q (⊢ '() '(* 2 3) q))
(run! 2 q (⊢ '() '(* #f 3) q))
(run! 2 q (⊢ '() '(if #t 2 3) q))
(run! 2 q (⊢ '() '(if #t 2 #f) q))
;; You only need the lambda and number case to be able to run this
(run! 2 q (⊢ '() '(λ (n) (* 2 3)) q))
(run! 2 q (⊢ '() '(λ (n) (zero? n)) q))
(run! 2 q (⊢ '((x . Nat)) 'x q))
