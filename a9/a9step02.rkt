#lang racket
(require rackunit)
(require "parenthec.rkt")
;Renaming the formal parameters and making the function calls anormal form.
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
  (lambda (*env-cps* *v* *k*)
    (match *env-cps*
      ; Extend Env
      [`(extend-env ,a^ ,env^)
       (if (zero? *v*)
           (let* ((*a-k* *k*)
                  (*v* a^))
          (apply-k *a-k* *v*))
           (let* ((*env-cps* env^)
                  (*v* (sub1 *v*))
                  (*k* *k*))
          (apply-env *env-cps* *v* *k*)
          ))]
      ; Empty Env
      [`(empty-env)
       (error "Unbound Identifier" *v*)]
      )))
(define empty-k
  (lambda ()
    `(empty-k)
    ))

(define apply-k
  (lambda (*a-k* *v*)
    (match *a-k*
      [`(empty-k) *v*]
      [`(make-mult-outer-k ,v^ ,env ,k)
       (let* ((*k* (make-mult-inner-k *v* k))
              (*cexpr* v^)
              (*env-cps* env))
         (value-of-cps *cexpr* *env-cps* *k*))]
      [`(make-mult-inner-k ,v^ ,k)
       (let* ((*v* (* v^ *v*))
              (*a-k* k))
       (apply-k *a-k* *v*))]
      [`(make-sub1-k ,k^)
       (let* ((*v* (sub1 *v*))
              (*a-k* k^))
       (apply-k *a-k* *v*))]
      [`(make-zero-k ,k^)
       (let* ((*v* (zero? *v*))
              (*a-k* k^))
       (apply-k *a-k* *v*))]
      [`(make-if-k ,conseq^ ,alt^ ,env^ ,k^)
       (if *v*
           (let* ((*cexpr* conseq^)
                  (*env-cps* env^)
                  (*k* k^))
           (value-of-cps *cexpr* *env-cps* *k*))
           (let* ((*cexpr* alt^)
                  (*env-cps* env^)
                  (*k* k^))
           (value-of-cps *cexpr* *env-cps* *k*)))]
      [`(make-pitch-outer-k ,v^ ,env^)
       (let* ((*cexpr* v^)
              (*env-cps* env^)
              (*k* (make-pitch-inner-k *v*)))
       (value-of-cps *cexpr* *env-cps* *k*))]
      [`(make-pitch-inner-k ,v^)
       (let* ((*a-k* v^)
              (*v* *v*))
       (apply-k *a-k* *v*))]
      [`(make-let-k ,v^ ,env^ ,k^)
       (let* ((*env-cps* (extend-env *v* env^))
              (*cexpr* v^)
              (*k* k^))
       (value-of-cps *cexpr* *env-cps* *k*))]
      [`(make-app-outer-k ,v^ ,env^ ,k^)
       (let* ((*k* (make-app-inner-k *v* k^))
              (*cexpr* v^)
              (*env-cps* env^))
       (value-of-cps *cexpr* *env-cps* *k*))]
      [`(make-app-inner-k ,v^ ,k^)
       (let* ((*clos* v^)
              (*v* *v*)
              (*k* k^))
       (apply-closure *clos* *v* *k*))]
      )))

(define apply-closure
  (λ (*clos* *v* *k*)
    (match *clos*
      (`(make-closure-ds ,x ,env)
       (let* ((*k* *k*)
              (*env-cps* (extend-env *v* env))
              (*cexpr* x))
       (value-of-cps *cexpr* *env-cps* *k*))))))
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

(define value-of-cps
  (lambda (*cexpr* *env-cps* *k*)
    (union-case *cexpr* expr
      [(const cexpr)
       (let* ((*a-k* *k*)
              (*v* cexpr))
       (apply-k *a-k* *v*))]
      [(mult x1 x2)
       (let* ((*k* (make-mult-outer-k x2 *env-cps* *k*))
              (*env-cps* *env-cps*)
              (*cexpr* x1))
       (value-of-cps *cexpr* *env-cps* *k*))]
      [(sub1 x)
       (let* ((*k* (make-sub1-k *k*))
              (*env-cps* *env-cps*)
              (*cexpr* x))
       (value-of-cps *cexpr* *env-cps* *k*))]
      [(zero x)
       (let* ((*k* (make-zero-k *k*))
              (*env-cps* *env-cps*)
              (*cexpr* x))
       (value-of-cps *cexpr* *env-cps* *k*))]
      [(if test conseq alt)
       (let* ((*k* (make-if-k conseq alt *env-cps* *k*))
              (*env-cps* *env-cps*)
              (*cexpr* test))
       (value-of-cps *cexpr* *env-cps* *k*))]
      [(catch body)
       (let* ((*env-cps* (extend-env *k* *env-cps*))
              (*k* *k*)
              (*cexpr* body))
       (value-of-cps *cexpr* *env-cps* *k*))]
      [(pitch k-exp v-exp)
       (let* ((*k* (make-pitch-outer-k v-exp *env-cps*))
              (*env-cps* *env-cps*)
              (*cexpr* k-exp))
       (value-of-cps *cexpr* *env-cps* *k*))]
      [(let e body)
       (let* ((*k* (make-let-k body *env-cps* *k*))
              (*env-cps* *env-cps*)
              (*cexpr* e))
       (value-of-cps *cexpr* *env-cps* *k*))]
      [(var v)
       (let* ((*k* *k*)
              (*env-cps* *env-cps*)
              (*v* v))
       (apply-env *env-cps* *v* *k*))]
      [(lambda body)
       (let* ((*v* (make-closure body *env-cps*))
              (*a-k* *k*))
       (apply-k *a-k* *v*))]
      [(app rator rand)
       (let* ((*k* (make-app-outer-k rand *env-cps* *k*))
              (*env-cps* *env-cps*)
              (*cexpr* rator))
       (value-of-cps *cexpr* *env-cps* *k*))]
      )))

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