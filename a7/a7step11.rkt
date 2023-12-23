#lang racket
(require rackunit)

(define empty-env
  (lambda ()
   `(empty-env)))

(define apply-env
  (lambda (env y k^)
    (match env
      ; Extend Env
      [`(extend-env ,a^ ,env^)
       (if (zero? y)
          (apply-k k^ a^)
          (apply-env env^ (sub1 y) k^)
          )]
      ; Empty Env
      [`(empty-env)
       (error "Unbound Identifier" y)]
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
      (_ (k v))
      )))

(define apply-closure
  (λ (clos y k)
    (match clos
      (`(make-closure-ds ,x ,env)
       (value-of-cps x (extend-env y env) k)))))
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
  (lambda (expr env-cps k)
    (match expr
      [`(const ,expr) (apply-k k expr)]
      [`(mult ,x1 ,x2)
       (value-of-cps x1 env-cps (make-mult-outer-k x2 env-cps k))]
      [`(sub1 ,x) (value-of-cps x env-cps (make-sub1-k k))]
      [`(zero ,x) (value-of-cps x env-cps (make-zero-k k))]
      [`(if ,test ,conseq ,alt) (value-of-cps test env-cps (make-if-k conseq alt env-cps k))]
      [`(catch ,body) (value-of-cps body (extend-env k env-cps) k)]
      [`(pitch ,k-exp ,v-exp) (value-of-cps k-exp env-cps (make-pitch-outer-k v-exp env-cps))]
      [`(let ,e ,body) (value-of-cps e env-cps (make-let-k body env-cps k))]
      [`(var ,y) (apply-env env-cps y k)]
      [`(lambda ,body)
       (apply-k k (make-closure body env-cps))]
      [`(app ,rator ,rand)
       (value-of-cps rator
                     env-cps (make-app-outer-k rand env-cps k)
                     )])))


(check-equal? (value-of-cps '(const 5) (empty-env) (empty-k)) 5)
(check-equal? (value-of-cps '(mult (const 5) (const 5)) (empty-env) (empty-k)) 25)
(check-equal? (value-of-cps '(sub1 (sub1 (const 5))) (empty-env) (empty-k)) 3)
(check-equal? (value-of-cps '(if (zero (const 0)) (mult (const 2) (const 2)) (const 3)) (empty-env) (empty-k)) 4)
(check-equal? (value-of-cps '(app (app (lambda (lambda (var 1))) (const 6)) (const 5)) (empty-env) (empty-k)) 6)
(check-equal? (value-of-cps '(app (lambda (app (lambda (var 1)) (const 6))) (const 5)) (empty-env) (empty-k)) 5)
(check-equal? (value-of-cps '(let (const 6) (const 4)) (empty-env) (empty-k)) 4)
(check-equal? (value-of-cps '(let (const 5) (var 0)) (empty-env) (empty-k)) 5)
(check-equal? (value-of-cps '(mult (const 5) (let (const 5) (var 0))) (empty-env) (empty-k)) 25)
(check-equal? (value-of-cps '(app (if (zero (const 4)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k)) 5)
(check-equal? (value-of-cps '(app (if (zero (const 0)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k)) 3)
(check-equal? (value-of-cps '(catch (pitch (pitch (var 0) (const 5)) (const 6))) (empty-env) (empty-k)) 5)
(check-equal? (value-of-cps '(catch (pitch (const 5) (pitch (var 0) (const 5)))) (empty-env) (empty-k)) 5)
(check-equal? (value-of-cps '(mult (const 3) (catch (pitch (const 5) (pitch (var 0) (const 5))))) (empty-env) (empty-k)) 15)
(check-equal? (value-of-cps '(if (zero (const 5)) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))) (const 4))
                            (empty-env)
                            (empty-k))
              4)
(check-equal? (value-of-cps '(if (zero (const 0)) (const 4) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))))
                            (empty-env)
                            (empty-k))
              4)
(check-equal? (value-of-cps '(app (lambda (app (app (var 0) (var 0)) (const 2)))
                                  (lambda
                                      (lambda
                                          (if (zero (var 0))
                                              (const 1)
                                              (app (app (var 1) (var 1)) (sub1 (var 0)))))))
                            (empty-env)
                            (empty-k))
              1)