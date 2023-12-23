#lang racket
(require rackunit)
(require "parenthec.rkt")
;The beginning
;Added define union for expr
(define-union expr
  (const cexp)
  (var n)
  (if test conseq alt)
  (mult nexp1 nexp2)
  (sub1 nexp)
  (zero nexp)
  (catch body)
  (pitch kexp vexp)
  (let exp body)
  (lambda body)
  (app rator rand))



(define empty-env
  (lambda ()
   `(empty-env)))

(define apply-env
  (lambda (env v k^)
    (match env
      ; Extend Env
      [`(extend-env ,a^ ,env^)
       (if (zero? v)
          (apply-k k^ a^)
          (apply-env env^ (sub1 v) k^)
          )]
      ; Empty Env
      [`(empty-env)
       (error "Unbound Identifier" v)]
      )))
(define empty-k
  (lambda ()
    `(empty-k)
    ))

(define apply-k
  (lambda (k v)
    (match k
      [`(empty-k) v]
      [`(make-mult-outer-k ,v^ ,env ,k)
       (value-of-cps v^ env (make-mult-inner-k v k))]
      [`(make-mult-inner-k ,v^ ,k)
       (apply-k k (* v^ v))]
      [`(make-sub1-k ,k^)
       (apply-k k^ (sub1 v))]
      [`(make-zero-k ,k^)
       (apply-k k^ (zero? v))]
      [`(make-if-k ,conseq^ ,alt^ ,env^ ,k^)
       (if v
           (value-of-cps conseq^ env^ k^)
           (value-of-cps alt^ env^ k^))]
      [`(make-pitch-outer-k ,v^ ,env^)
       (value-of-cps v^ env^ (make-pitch-inner-k v))]
      [`(make-pitch-inner-k ,v^)
       (apply-k v^ v)]
      [`(make-let-k ,v^ ,env^ ,k^)
       (value-of-cps v^ (extend-env v env^) k^)]
      [`(make-app-outer-k ,v^ ,env^ ,k^)
       (value-of-cps v^ env^ (make-app-inner-k v k^))]
      [`(make-app-inner-k ,v^ ,k^)
       (apply-closure v^ v k^)]
      )))

(define apply-closure
  (λ (clos v k)
    (match clos
      (`(make-closure-ds ,x ,env)
       (value-of-cps x (extend-env v env) k)))))
(define make-closure
  (λ (x env)
    `(make-closure-ds ,x ,env)))

(define make-mult-outer-k
  (λ (v^ env k)
    `(make-mult-outer-k ,v^ ,env ,k)))

(define make-mult-inner-k
  (λ (v^ k)
    `(make-mult-inner-k ,v^ ,k)))

(define make-sub1-k
  (λ (k^)
    `(make-sub1-k ,k^)))

(define make-zero-k
  (λ (k^)
    `(make-zero-k ,k^)))

(define make-if-k
  (λ (conseq^ alt^ env^ k^)
    `(make-if-k ,conseq^ ,alt^ ,env^ ,k^)))

(define make-pitch-outer-k
  (λ (v^ env^)
    `(make-pitch-outer-k ,v^ ,env^)))

(define make-pitch-inner-k
  (λ (v^)
    `(make-pitch-inner-k ,v^)))

(define make-let-k
  (λ (v^ env^ k^)
    `(make-let-k ,v^ ,env^ ,k^)))

(define make-app-outer-k
  (λ (v^ env^ k^)
    `(make-app-outer-k ,v^ ,env^ ,k^)))

(define make-app-inner-k
  (λ (v^ k^)
    `(make-app-inner-k ,v^ ,k^)))

(define extend-env
  (λ (a^ env-cps^)
    `(extend-env ,a^ ,env-cps^)))
;Changed the interpreter to use a union-case instead of a match
(define value-of-cps
  (lambda (cexpr env-cps k)
    (union-case cexpr expr
      [(const cexpr) (apply-k k cexpr)]
      [(mult x1 x2)
       (value-of-cps x1 env-cps (make-mult-outer-k x2 env-cps k))]
      [(sub1 x) (value-of-cps x env-cps (make-sub1-k k))]
      [(zero x) (value-of-cps x env-cps (make-zero-k k))]
      [(if test conseq alt) (value-of-cps test env-cps (make-if-k conseq alt env-cps k))]
      [(catch body) (value-of-cps body (extend-env k env-cps) k)]
      [(pitch k-exp v-exp) (value-of-cps k-exp env-cps (make-pitch-outer-k v-exp env-cps))]
      [(let e body) (value-of-cps e env-cps (make-let-k body env-cps k))]
      [(var y) (apply-env env-cps y k)]
      [(lambda body)
       (apply-k k (make-closure body env-cps))]
      [(app rator rand)
       (value-of-cps rator
                     env-cps (make-app-outer-k rand env-cps k)
                     )])))
;Added this main
(define main
  (lambda ()
    (value-of-cps
     (expr_let
      (expr_lambda
       (expr_lambda
        (expr_if
         (expr_zero (expr_var 0))
         (expr_const 1)
         (expr_mult
          (expr_var 0)
          (expr_app
           (expr_app (expr_var 1) (expr_var 1))
           (expr_sub1 (expr_var 0)))))))
      (expr_mult
       (expr_catch
        (expr_app
         (expr_app (expr_var 1) (expr_var 1))
         (expr_pitch
          (expr_var 0)
          (expr_app
           (expr_app (expr_var 1) (expr_var 1))
           (expr_const 4)))))
       (expr_const 5)))
     (empty-env)
     (empty-k))))

(main)