#lang racket
(require racket/trace)
(require rackunit)

;Part 1
(define last-non-zero
  (lambda (ls)
    (let/cc k
      (letrec
          ((last-non-zero
            (lambda (ls)
              (cond
                ((empty? ls) '())
                ((eqv? (car ls) 0) (k (last-non-zero (cdr ls))))
                (else
                 (cons (car ls) (last-non-zero (cdr ls))))))))
        (last-non-zero ls)))))

;((eqv? (last-non-zero (cdr ls)) '())  ls)
;(trace last-non-zero)
;(last-non-zero '(0)) ;()

;(last-non-zero '(1 2 3 0 4 5)) ;(4 5)

;(last-non-zero '(1 0 2 3 0 4 5)) ;(4 5)

;(last-non-zero '(1 2 3 4 5)) ;(1 2 3 4 5)

;Part 2
(define idx
  (λ (y l)
    (match l
      (`()
       y)
      (`(,a . ,d)
       #:when (eqv? y a)
       0)
      (_ (match (idx y (cdr l))
           (`,y #:when (number? y) (add1 y))
           (_ y)
           )))))

(define lex
  (λ (e acc)
    (match e
      (`,y
       #:when (symbol? y)
       (match (idx y acc)
         (`,y #:when (number? y) (list 'var y))
         (_ y)))
      (`(lambda (,x) ,b)
       #:when (symbol? x)
       `(lambda ,(lex b (cons x acc))))
      [`(zero? ,nexp) `(zero ,(lex nexp acc))]
      [`(* ,nexp1 ,nexp2) `(mult ,(lex nexp1 acc) ,(lex nexp2 acc))]
      [`(catch ,c-name ,c-exp)
       #:when (symbol? c-name)
       `(catch ,(lex c-exp (cons c-name acc)))]
      [`(pitch ,exp1 ,c-exp)
       `(pitch ,(lex exp1 acc) ,(lex c-exp acc))]
      [`(,rator ,rand) `(app ,(lex rator acc) ,(lex rand acc))])))


;Part 3 step 1
#|
(define value-of-cps
  (lambda (expr env-cps k)
    (match expr
      [`(const ,expr) (k expr)]
      [`(mult ,x1 ,x2)
       (value-of-cps x1 env-cps
                     (λ (v)
                       (value-of-cps x2 env-cps (λ (w)
                                              (k (* v w))))))]
      [`(sub1 ,x) (value-of-cps x env-cps (λ (v) (k (sub1 v))))]
      [`(zero ,x) (value-of-cps x env-cps (λ (v) (k (zero? v))))]
      [`(if ,test ,conseq ,alt) (value-of-cps test env-cps (lambda (v) (if v (value-of-cps conseq env-cps k) (value-of-cps alt env-cps k))))]
      [`(catch ,body) (value-of-cps body (lambda (y k^^) (if (zero? y) (k^^ k) (env-cps (sub1 y) k)))
                                    k)]
      [`(pitch ,k-exp ,v-exp) (value-of-cps k-exp env-cps (λ (v) (value-of-cps v-exp env-cps (λ (w) (v w)))))]
      [`(let ,e ,body) (value-of-cps e env-cps (λ (a) (value-of-cps body (lambda (y k^^) (if (zero? y) (k^^ a) (env-cps (sub1 y) k^^))) k)))]
      [`(var ,y) (env-cps y k)]
      [`(lambda ,body) (k (lambda (a k^)
                         (value-of-cps body (lambda (y k^^) (if (zero? y) (k^^ a) (env-cps (sub1 y) k^^))) k^)))] 
      [`(app ,rator ,rand)
       (value-of-cps rator
                     env-cps
                     (lambda (v)
                       (value-of-cps rand env-cps
                                     (lambda (w) (v w k)))))])))
 
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
;(trace value-of-cps)
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
|#
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





;;Brainteaser
(define-syntax cons$
  (syntax-rules ()
    ((cons$ x y) (cons x (delay y)))))
 
(define car$ car)
 
(define cdr$
  (lambda ($) (force (cdr $))))

(define inf-1s (cons$ 1 inf-1s))


(define take$
  (lambda (n $)
    (cond
      ((zero? n) '())
      (else (cons (car$ $)
              (let ((n- (sub1 n)))
                (cond
                  ((zero? n-) '())
                  (else (take$ n- (cdr$ $))))))))))


;(take$ 5 inf-1s) ;(1 1 1 1 1)

;(take$ 10 inf-1s) ;(1 1 1 1 1 1 1 1 1 1)

(define worlds-worst-random
    (delay (random 4)))
;(force worlds-worst-random) ;0 1 or 2
;(force worlds-worst-random) ;0 1 or 2
;(force worlds-worst-random) ;0 1 or 2

;Tribonacci

(define cons-trib$
  (λ (a b c)
    (cons$ (+ a b c) (cons-trib$ b c (+ a b c)))))
;(define cons-trib2$
;  (λ (a b c)
;    (cons$ (+ a b c) (cons-trib2$ b c (+ a b c)))))

;(define trib$2 (cons$ 0 (cons$ 1 (cons$ 1 (cons-trib$2 0 1 1)))))
(define trib$ (cons$ 0 (cons-trib$ 1 0 0)))
;(define trib$2 (cons$ 0 (cons$ 0 (cons-trib$ 0 0 1))))

;(trace take$)
(car$ trib$)
(car$ (cdr$ trib$))
(take$ 12 trib$)
;(take$ 7 trib$2)
;(take$ 7 trib$2)

