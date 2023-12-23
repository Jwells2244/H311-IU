#lang racket
(require rackunit)
(require "parenthec.rkt")

(define-registers *env-cps* *k* *a-k* *v* *cexpr* *clos*)

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

(define-union clos
  (make-closure x env))

(define-union envr
  (extend-env a^ env^)
  (empty-env))

;Can get rid of this helper function. as well as the extend-env function
;(define empty-env
;  (lambda ()
;    `(empty-env)))
;(define extend-env
;  (λ (a^ env-cps^)
;    `(extend-env ,a^ ,env-cps^)))
(define apply-env
  (lambda ()
    (union-case *env-cps* envr
      ; Extend Env
      [(extend-env a^ env^)
       (if (zero? *v*)
           (begin (set! *a-k* *k*)
                  (set! *v* a^)
                  (apply-k))
           (begin (set! *env-cps* env^)
                  (set! *v* (sub1 *v*))
                  (set! *k* *k*)
                  (apply-env)
                  ))]
      ; Empty Env
      [(empty-env)
       (error "Unbound Identifier" *v*)]
      )))
(define apply-closure
  (λ ()
    (union-case *clos* clos
      ((make-closure x env)
       (begin (set! *k* *k*)
              (set! *env-cps* (envr_extend-env *v* env))
              (set! *cexpr* x)
              (value-of-cps))))))


(define apply-k
  (lambda ()
    (match *a-k*
      [`(empty-k) *v*]
      [`(make-mult-outer-k ,v^ ,env ,k)
       (begin (set! *k* (make-mult-inner-k *v* k))
              (set! *cexpr* v^)
              (set! *env-cps* env)
              (value-of-cps))]
      [`(make-mult-inner-k ,v^ ,k)
       (begin (set! *v* (* v^ *v*))
              (set! *a-k* k)
              (apply-k))]
      [`(make-sub1-k ,k^)
       (begin (set! *v* (sub1 *v*))
              (set! *a-k* k^)
              (apply-k))]
      [`(make-zero-k ,k^)
       (begin (set! *v* (zero? *v*))
              (set! *a-k* k^)
              (apply-k))]
      [`(make-if-k ,conseq^ ,alt^ ,env^ ,k^)
       (if *v*
           (begin (set! *cexpr* conseq^)
                  (set! *env-cps* env^)
                  (set! *k* k^)
                  (value-of-cps))
           (begin (set! *cexpr* alt^)
                  (set! *env-cps* env^)
                  (set! *k* k^)
                  (value-of-cps)))]
      [`(make-pitch-outer-k ,v^ ,env^)
       (begin (set! *cexpr* v^)
              (set! *env-cps* env^)
              (set! *k* (make-pitch-inner-k *v*))
              (value-of-cps))]
      [`(make-pitch-inner-k ,v^)
       (begin (set! *a-k* v^)
              (set! *v* *v*)
              (apply-k))]
      [`(make-let-k ,v^ ,env^ ,k^)
       (begin (set! *env-cps* (envr_extend-env *v* env^))
              (set! *cexpr* v^)
              (set! *k* k^)
              (value-of-cps))]
      [`(make-app-outer-k ,v^ ,env^ ,k^)
       (begin (set! *k* (make-app-inner-k *v* k^))
              (set! *cexpr* v^)
              (set! *env-cps* env^)
              (value-of-cps))]
      [`(make-app-inner-k ,v^ ,k^)
       (begin (set! *clos* v^)
              (set! *v* *v*)
              (set! *k* k^)
              (apply-closure))]
      )))


(define empty-k
  (lambda ()
    `(empty-k)
    ))
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


(define value-of-cps
  (lambda ()
    (union-case *cexpr* expr
                [(const cexpr)
                 (begin (set! *a-k* *k*)
                        (set! *v* cexpr)
                        (apply-k))]
                [(mult x1 x2)
                 (begin (set! *k* (make-mult-outer-k x2 *env-cps* *k*))
                        (set! *env-cps* *env-cps*)
                        (set! *cexpr* x1)
                        (value-of-cps))]
                [(sub1 x)
                 (begin (set! *k* (make-sub1-k *k*))
                        (set! *env-cps* *env-cps*)
                        (set! *cexpr* x)
                        (value-of-cps))]
                [(zero x)
                 (begin (set! *k* (make-zero-k *k*))
                        (set! *env-cps* *env-cps*)
                        (set! *cexpr* x)
                        (value-of-cps))]
                [(if test conseq alt)
                 (begin (set! *k* (make-if-k conseq alt *env-cps* *k*))
                        (set! *env-cps* *env-cps*)
                        (set! *cexpr* test)
                        (value-of-cps))]
                [(catch body)
                 (begin (set! *env-cps* (envr_extend-env *k* *env-cps*))
                        (set! *k* *k*)
                        (set! *cexpr* body)
                        (value-of-cps))]
                [(pitch k-exp v-exp)
                 (begin (set! *k* (make-pitch-outer-k v-exp *env-cps*))
                        (set! *env-cps* *env-cps*)
                        (set! *cexpr* k-exp)
                        (value-of-cps))]
                [(let e body)
                 (begin (set! *k* (make-let-k body *env-cps* *k*))
                        (set! *env-cps* *env-cps*)
                        (set! *cexpr* e)
                        (value-of-cps))]
                [(var v)
                 (begin (set! *k* *k*)
                        (set! *env-cps* *env-cps*)
                        (set! *v* v)
                        (apply-env))]
                [(lambda body)
                 (begin (set! *v* (clos_make-closure body *env-cps*))
                        (set! *a-k* *k*)
                        (apply-k))]
                [(app rator rand)
                 (begin (set! *k* (make-app-outer-k rand *env-cps* *k*))
                        (set! *env-cps* *env-cps*)
                        (set! *cexpr* rator)
                        (value-of-cps))]
                )))

(define main
  (lambda ()
    (begin (set! *cexpr* (expr_let
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
                           (expr_const 5))))
           (set! *env-cps* (envr_empty-env))
           (set! *k* (empty-k))
           (value-of-cps))))

(main)