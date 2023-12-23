;#lang racket
;(require "parenthec.rkt")
;Rip lang racket and require parenthec, and (main) too. Can no longer run and produce 120 obviously.
(define-registers *env-cps* *k* *a-k* *v* *cexpr* *clos*)
(define-program-counter *pc*)

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

(define-union envr
  (extend-env a^ env^)
  (empty-env))

(define-union kt
  (make-mult-outer-k v^ env k)
  (make-mult-inner-k v^ k)
  (make-sub1-k k^)
  (make-zero-k k^)
  (make-if-k conseq^ alt^ env^ k^)
  (make-pitch-outer-k v^ env^)
  (make-pitch-inner-k v^)
  (make-let-k v^ env^ k^)
  (make-app-outer-k v^ env^ k^)
  (make-app-inner-k v^ k^)
  (empty-k jumpout))

(define-union clos
  (make-closure x env))

(define-label apply-env
    (union-case *env-cps* envr
      [(extend-env a^ env^)
       (if (zero? *v*)
           (begin (set! *a-k* *k*)
                  (set! *v* a^)
                  (set! *pc* apply-k))
           (begin (set! *env-cps* env^)
                  (set! *v* (sub1 *v*))
                  (set! *k* *k*)
                  (set! *pc* apply-env)
                  ))]
      [(empty-env)
       (error "Unbound Identifier" *v*)]
      ))
(define-label apply-closure
    (union-case *clos* clos
      ((make-closure x env)
       (begin (set! *k* *k*)
              (set! *env-cps* (envr_extend-env *v* env))
              (set! *cexpr* x)
              (set! *pc* value-of-cps)))))
(define-label apply-k
    (union-case *a-k* kt
      [(empty-k jumpout) (dismount-trampoline jumpout)]
      [(make-mult-outer-k v^ env k)
       (begin (set! *k* (kt_make-mult-inner-k *v* k))
              (set! *cexpr* v^)
              (set! *env-cps* env)
              (set! *pc* value-of-cps))]
      [(make-mult-inner-k v^ k)
       (begin (set! *v* (* v^ *v*))
              (set! *a-k* k)
              (set! *pc* apply-k))]
      [(make-sub1-k k^)
       (begin (set! *v* (sub1 *v*))
              (set! *a-k* k^)
              (set! *pc* apply-k))]
      [(make-zero-k k^)
       (begin (set! *v* (zero? *v*))
              (set! *a-k* k^)
              (set! *pc* apply-k))]
      [(make-if-k conseq^ alt^ env^ k^)
       (if *v*
           (begin (set! *cexpr* conseq^)
                  (set! *env-cps* env^)
                  (set! *k* k^)
                  (set! *pc* value-of-cps))
           (begin (set! *cexpr* alt^)
                  (set! *env-cps* env^)
                  (set! *k* k^)
                  (set! *pc* value-of-cps)))]
      [(make-pitch-outer-k v^ env^)
       (begin (set! *cexpr* v^)
              (set! *env-cps* env^)
              (set! *k* (kt_make-pitch-inner-k *v*))
              (set! *pc* value-of-cps))]
      [(make-pitch-inner-k v^)
       (begin (set! *a-k* v^)
              (set! *v* *v*)
              (set! *pc* apply-k))]
      [(make-let-k v^ env^ k^)
       (begin (set! *env-cps* (envr_extend-env *v* env^))
              (set! *cexpr* v^)
              (set! *k* k^)
              (set! *pc* value-of-cps))]
      [(make-app-outer-k v^ env^ k^)
       (begin (set! *k* (kt_make-app-inner-k *v* k^))
              (set! *cexpr* v^)
              (set! *env-cps* env^)
              (set! *pc* value-of-cps))]
      [(make-app-inner-k v^ k^)
       (begin (set! *clos* v^)
              (set! *v* *v*)
              (set! *k* k^)
              (set! *pc* apply-closure))]
      ))


(define-label value-of-cps
    (union-case *cexpr* expr
                [(const cexpr)
                 (begin (set! *a-k* *k*)
                        (set! *v* cexpr)
                        (set! *pc* apply-k))]
                [(mult x1 x2)
                 (begin (set! *k* (kt_make-mult-outer-k x2 *env-cps* *k*))
                        (set! *env-cps* *env-cps*)
                        (set! *cexpr* x1)
                        (set! *pc* value-of-cps))]
                [(sub1 x)
                 (begin (set! *k* (kt_make-sub1-k *k*))
                        (set! *env-cps* *env-cps*)
                        (set! *cexpr* x)
                        (set! *pc* value-of-cps))]
                [(zero x)
                 (begin (set! *k* (kt_make-zero-k *k*))
                        (set! *env-cps* *env-cps*)
                        (set! *cexpr* x)
                        (set! *pc* value-of-cps))]
                [(if test conseq alt)
                 (begin (set! *k* (kt_make-if-k conseq alt *env-cps* *k*))
                        (set! *env-cps* *env-cps*)
                        (set! *cexpr* test)
                        (set! *pc* value-of-cps))]
                [(catch body)
                 (begin (set! *env-cps* (envr_extend-env *k* *env-cps*))
                        (set! *k* *k*)
                        (set! *cexpr* body)
                        (set! *pc* value-of-cps))]
                [(pitch k-exp v-exp)
                 (begin (set! *k* (kt_make-pitch-outer-k v-exp *env-cps*))
                        (set! *env-cps* *env-cps*)
                        (set! *cexpr* k-exp)
                        (set! *pc* value-of-cps))]
                [(let e body)
                 (begin (set! *k* (kt_make-let-k body *env-cps* *k*))
                        (set! *env-cps* *env-cps*)
                        (set! *cexpr* e)
                        (set! *pc* value-of-cps))]
                [(var v)
                 (begin (set! *k* *k*)
                        (set! *env-cps* *env-cps*)
                        (set! *v* v)
                        (set! *pc* apply-env))]
                [(lambda body)
                 (begin (set! *v* (clos_make-closure body *env-cps*))
                        (set! *a-k* *k*)
                        (set! *pc* apply-k))]
                [(app rator rand)
                 (begin (set! *k* (kt_make-app-outer-k rand *env-cps* *k*))
                        (set! *env-cps* *env-cps*)
                        (set! *cexpr* rator)
                        (set! *pc* value-of-cps))]
                ))

(define-label main
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
           (set! *pc* value-of-cps)
           (mount-trampoline kt_empty-k *k* *pc*)
           (printf "Value of expression is ~s\n" *v*)))

;(main)