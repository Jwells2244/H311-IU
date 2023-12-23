#lang racket
(require rackunit)
(define empty-env
  (lambda ()
    (lambda (y)
      (error 'value-of "unbound identifier"))))
 
(define empty-k
  (lambda ()
    (lambda (v)
      v)))

(define apply-env
  (lambda (env-cps e k^)
    (env-cps e k^)))

(define apply-k
  (lambda (k^ v)
    (k^ v)))

(define apply-closure
  (lambda (rator rand k^)
    (rator rand k^)))

(define value-of-cps
  (lambda (expr env-cps k)
    (match expr
      [`(const ,expr) (apply-k k expr)]
      [`(mult ,x1 ,x2)
       (value-of-cps x1 env-cps
                     (λ (v)
                       (value-of-cps x2 env-cps (λ (w)
                                              (apply-k k (* v w))))))]
      [`(sub1 ,x) (value-of-cps x env-cps (λ (v) (k (sub1 v))))]
      [`(zero ,x) (value-of-cps x env-cps (λ (v) (k (zero? v))))]
      [`(if ,test ,conseq ,alt) (value-of-cps test env-cps (lambda (v) (if v (value-of-cps conseq env-cps k) (value-of-cps alt env-cps k))))]
      [`(catch ,body) (value-of-cps body (lambda (y k^^) (if (zero? y) (apply-k k^^ k) (apply-env env-cps (sub1 y) k)))
                                    k)]
      [`(pitch ,k-exp ,v-exp) (value-of-cps k-exp env-cps (λ (v) (value-of-cps v-exp env-cps (λ (w) (v w)))))]
      [`(let ,e ,body) (value-of-cps e env-cps (λ (a) (value-of-cps body (lambda (y k^^) (if (zero? y) (apply-k k^^ a) (apply-env env-cps (sub1 y) k^^))) k)))]
      [`(var ,y) (apply-env env-cps y k)]
      [`(lambda ,body) (apply-k k (lambda (a k^)
                         (value-of-cps body (lambda (y k^^) (if (zero? y) (apply-k k^^ a) (apply-env env-cps (sub1 y) k^^))) k^)))] 
      [`(app ,rator ,rand)
       (value-of-cps rator
                     env-cps
                     (lambda (v)
                       (value-of-cps rand env-cps
                                     (lambda (w) (apply-closure v w k)))))])))


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