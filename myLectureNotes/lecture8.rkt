#lang racket


(define val-of
  (λ (e env)
    (match e
      (`,y #:when (symbol? y) (env y))
      (`,n #:when (number? n) n)
      (`(+ ,nexpr1 ,nexpr2)
       (+ (val-of nexpr1 env)
          (val-of nexpr2 env)))
      (`(λ (,x) ,body)
       #:when (symbol? x)
       (λ (arg)
         (val-of body (λ (y)
                        (cond
                          ((eqv? y x) arg)
                          (else (env y)))))))
      (`(,rator ,rand)
       ((val-of rator env) (val-of rand env))))))
