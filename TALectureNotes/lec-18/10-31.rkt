#lang racket
(require "mk-new.rkt")
(require "numbers-new.rkt")

;
(define apply-env
  (λ (env y)
    (match env
      (`() (error "Unbound variable" y))
      (`((,x . ,v) . ,env^)
       #:when (eqv? x y)
       v)
      (`((,x . ,v) . ,env^)
       (apply-env env^ y)))))

(define extend-env
  (λ (env x v)
    `((,x . ,v) . ,env)))

(define value-of-list
  (λ (exprs env)
    (match exprs
      (`() '())
      (`(,e1 . ,exprs^)
       (cons (value-of e1 env)
             (value-of-list exprs^ env))))))

(define value-of
  (λ (expr env)
    (match expr
      (`(quote ,x) x)
      (`(list . ,exprs)
       (value-of-list exprs env))
      (`,y
       #:when (symbol? y)
       (apply-env env y))
      (`(λ (,x) ,body)
       `(clos ,x ,body ,env))
      (`(,rator ,rand)
       (match (value-of rator env)
         (`(clos ,x ,body ,env^)
          (match (value-of rand env)
            (`,arg (value-of body (extend-env env^ x arg))))))))))

;(value-of '(λ (x) x) '())

(define lookup
  (λ (senv venv y)
    (match `(,senv ,venv)
      (`(() ())
       (error "Unbound variable" y))
      (`((,sa . ,sd) (,va . ,vd))
       (cond
         ((eqv? sa y) va)
         (else (lookup sd vd y)))))))


;;; For a branch that cannot succeed, we can
;;; eliminate it from the relation.
(defrel (lookupo senv venv y o)
  (fresh (sa sd va vd)
    (conj
      (== `(,sa . ,sd) senv)
      (== `(,va . ,vd) venv)
      (disj
        (conj (== sa y) (== va o))
        (conj (=/= sa y) (lookupo sd vd y o))))))

;(run! 1 q (lookupo '(a b c a d) '(3 5 1 8 13) 'c q))
;(run! 2 q (lookupo '(a b c a d) '(3 5 1 8 13) 'a q))

;;; Because we are using "clos" and some other
;;; special keywords in our expressions, we want
;;; to exclude the case that these special
;;; keywords are used as bound variables in our
;;; environments.
(define not-in-env
  (λ (y vars)
    (match vars
      (`() #t)
      (`(,a . ,d)
       #:when (eqv? a y)
       #f)
      (`(,a . ,d) (not-in-env y d)))))

(defrel (not-in-envo y vars)
  (disj
    (conj
      (== '() vars)
      succeed)
    (fresh (a d)
      (conj
        (== `(,a . ,d) vars)
        (=/= a y)
        (not-in-envo y d)))))

(run! 1 q (not-in-envo q '(clos x y z d)))
(run! 1 q (not-in-envo 'clos `(,q x y z d)))

(define valof-list
  (λ (exprs vars vals)
    (match exprs
      (`() '())
      (`(,e1 . ,exprs^)
       (cons (valof e1 vars vals)
             (valof-list exprs^ vars vals))))))

(defrel (valof-listo exprs vars vals o)
  (disj
    (conj (== '() exprs)
          (== '() o))
    (fresh (e1 exprs^ car-res cdr-res)
      (conj
        (== `(,e1 . ,exprs^) exprs)
        (== `(,car-res . ,cdr-res) o)
        (valofo e1 vars vals car-res)
        (valof-listo exprs^ vars vals cdr-res)))))

(define (valof expr vars vals)
  (match expr
    (`(quote ,x)
     #:when (not-in-env 'quote vars)
     x)
    (`(list . ,exprs)
     #:when (not-in-env 'list vars)
     (valof-list exprs vars vals))
    (`,y
     #:when (symbol? y)
     (lookup vars vals y))
    (`(λ (,x) ,body)
     #:when (and (symbol? x) (not-in-env 'λ vars))
     `(clos ,x ,body ,vars ,vals))
    (`(,rator ,rand)
     (match (valof rator vars vals)
       (`(clos ,x ,body ,vars^ ,vals^)
        (match (valof rand vars vals)
          (`,arg (valof body `(,x . ,vars^) `(,arg . ,vals^)))))))))

(defrel (valofo expr vars vals o)
  (disj
    (fresh (x)
      (conj
        (== `(quote ,x) expr)
        (== x o)
        (absento 'clos x)
        (not-in-envo 'quote vars)))
    (fresh (exprs)
      (conj
        (== `(list . ,exprs) expr)
        (absento 'clos exprs)
        (not-in-envo 'list vars)
        (valof-listo exprs vars vals o)))
    (fresh (y)
      (conj
        (== y expr)
        (symbolo y)
        (lookupo vars vals y o)))
    (fresh (x body)
      (conj
        (== `(λ (,x) ,body) expr)
        (symbolo x)
        (not-in-envo 'λ vars)
        (== `(clos ,x ,body ,vars ,vals) o)))
    (fresh (rator rand rator-res x body vars^ vals^ rand-res)
      (conj
        (== `(,rator ,rand) expr)
        (== `(clos ,x ,body ,vars^ ,vals^) rator-res)
        ;; move recursive calls to bottom
        (valofo rator vars vals rator-res)
        (valofo rand vars vals rand-res)
        (valofo body `(,x . ,vars^) `(,rand-res . ,vals^) o)))))

(valof '(λ (x) x) '() '())

(run! 1 q (valofo 'x '(a b x d) '(3 1 6 4) q))
(run! 1 q (valofo '(λ (x) x) '(a b x d) '(3 1 6 4) q))

(run! 10 q (valofo q '() '() q))

(define q
  ((λ (_0) (list _0 (list (quote quote) _0)))
   (quote (λ (_0) (list _0 (list (quote quote) _0))))))
#;
(equal? q (eval q))
#;
(run! 1 (p q)
  (=/= p q)
  (valofo p '() '() q)
  (valofo q '() '() p))
#;
(run! 1 (p q r)
  (=/= p q)
  (=/= q r)
  (valofo p '() '() q)
  (valofo q '() '() r)
  (valofo r '() '() p))

(define p1
  (quote
   (quote
    ((λ (_0)
       (list (quote quote) (list (quote quote) (list _0 (list (quote quote) _0)))))
     (quote (λ (_0) (list (quote quote) (list (quote quote) (list _0 (list (quote quote) _0))))))))))

(define q1
  (quote
   ((λ (_0)
      (list (quote quote)
            (list (quote quote)
                  (list _0 (list (quote quote) _0)))))
    (quote (λ (_0) (list (quote quote) (list (quote quote) (list _0 (list (quote quote) _0)))))))))

(define r1
  ((λ (_0) (list (quote quote) (list (quote quote) (list _0 (list (quote quote) _0)))))
   (quote (λ (_0) (list (quote quote) (list (quote quote) (list _0 (list (quote quote) _0))))))))

;(equal? (eval p1) q1)
;(equal? (eval q1) r1)
;(equal? (eval r1) p1)
