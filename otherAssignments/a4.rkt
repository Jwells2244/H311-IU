#lang racket
(define value-of-ds
  (λ (e env)
    (match e
      (`,n #:when (number? n) n)
      (`,b #:when (boolean? b) b)
      (`,y
       #:when (symbol? y)
       (apply-env-ds env y))
      (`(lambda (,x) ,body)
       #:when (symbol? x)
       (make-closure-ds x body env))
      (`(zero? ,z) (zero? (value-of-ds z env)))
      (`(sub1 ,s) (sub1 (value-of-ds s env)))
      (`(* ,e1 ,e2)
       (* (value-of-ds e1 env) (value-of-ds e2 env))
       )
      (`(if ,c ,t ,f) (if (value-of-ds c env) (value-of-ds t env) (value-of-ds f env)))
      (`(let ((,x ,v)) ,body)
       #:when (symbol? x)
       (value-of-ds body (extend-env-ds x (value-of-ds v env) env)))
      (`(,rator ,rand)
       (apply-closure-ds
        (value-of-ds rator env)
        (value-of-ds rand env))))))

;; A2 -- Walk-Symbol
(define walk-symbol
  (λ (x s)
    (cond
      ((eqv? #f (assv x s)) x)
      (else (walk-symbol (cdr (assv x s)) s)))))


(define apply-env-ds
  (λ (env y)
    (car (walk-symbol y env))))

(define extend-env-ds
  (λ (x arg env)
    (cons (cons x (cons arg '())) env)))

(define empty-env-ds
  (λ () `()))

(define apply-closure-ds
  (λ (clos arg)
    (match clos
      (`(make-closure-ds ,x ,body ,env)
       (value-of-ds body (extend-env-ds x arg env)))
      (_ (clos arg)))))

(define make-closure-ds
  (λ (x body env)
    `(make-closure-ds ,x ,body ,env)))

(value-of-ds
   '((lambda (x) (if (zero? x)
                     #t
                     #f))
     0)
   (empty-env-ds))

(value-of-ds
   '((lambda (x) (if (zero? x)
                     12
                     47))
     0)
   (empty-env-ds))
(value-of-ds
   '(let ([y (* 3 4)])
      ((lambda (x) (* x y)) (sub1 6)))
   (empty-env-ds))
(value-of-ds
   '(let ([x (* 2 3)])
      (let ([y (sub1 x)])
        (* x y)))
   (empty-env-ds))
(value-of-ds
   '(let ([x (* 2 3)])
      (let ([x (sub1 x)])
        (* x x)))
   (empty-env-ds))
(value-of-ds
   '(((lambda (f)
        (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
      (lambda (f)
        (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
     5)
   (empty-env-ds))