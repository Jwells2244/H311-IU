#lang racket
(require rackunit)

(define empty-env
  (lambda ()
   `(empty-env)))


(define empty-k
  (lambda ()
    (lambda (v)
      v)))

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

(define apply-k
  (lambda (k^ v)
    (k^ v)))

(define apply-closure
  (λ (clos y k)
    (match clos
      (`(make-closure-ds ,x ,env)
       (value-of-cps x (extend-env y env) k)))))
(define make-closure
  (λ (x env)
    `(make-closure-ds ,x ,env)))

(define make-sub1-k
  (λ (k)
    (λ (v)
      (apply-k k (sub1 v)))))

(define make-zero-k
  (λ (k)
    (λ (v)
      (apply-k k (zero? v)))))

(define make-mult-outer-k
  (λ (x env k)
    (λ (v)
      (value-of-cps x env (make-mult-inner-k v k)))))

(define make-mult-inner-k
  (λ (v k)
    (λ (w)
      (apply-k k (* v w)))))
(define make-if-k
  (λ (conseq alt env k)
    (λ (v)
      (if v (value-of-cps conseq env k) (value-of-cps alt env k)))))

(define make-pitch-outer-k
  (λ (v env)
    (λ (k)
      (value-of-cps v env (make-pitch-inner-k k)))))

(define make-pitch-inner-k
  (λ (k)
    (λ (v)
      (apply-k k v))))
(define make-let-k
  (λ (body env k)
    (λ (a)
      (value-of-cps body (extend-env a env) k))))

(define extend-env
  (λ (a^ env-cps^)
    `(extend-env ,a^ ,env-cps^)))

(define make-app-outer-k
  (λ (rand env k)
    (λ (v)
      (value-of-cps rand env (make-app-inner-k v k)))))

(define make-app-inner-k
  (λ (v k)
    (λ (w)
      (apply-closure v w k))))

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