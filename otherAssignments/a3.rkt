#lang racket

;;Question 1 This is probably not right because they specifically ask you to not go up the tree to find the idx
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
  (λ (e lenv)
    (match e
      (`,y
       #:when (symbol? y)
       (match (idx y lenv)
         (`,y #:when (number? y) (list 'var y))
         (_ y)))
      (`(lambda (,x) ,b)
       #:when (symbol? x)
       `(lambda ,(lex b (cons x lenv))))
      (`(,rator ,rand)
       `(,(lex rator lenv) ,(lex rand lenv))))))

;;(lex '(lambda (x) x)'())
;;(lex '(lambda (y) (lambda (x) y))'())
;;(lex '(lambda (y) (lambda (x) (x y)))'())
;;(lex '(lambda (x) (lambda (x) (x x))) 
;;       '())
;;(lex '(lambda (x) (lambda (x) (y x)))
;;     '())
;;(lex '(lambda (y) ((lambda (x) (x y)) (lambda (c) (lambda (d) (y c)))))
;;       '())
;;(lex '(lambda (a)
;          (lambda (b)
;            (lambda (c)
;              (lambda (a)
;                (lambda (b)
;                  (lambda (d)
;                    (lambda (a)
;                      (lambda (e)
;                        (((((a b) c) d) e) a)))))))))
;       '())
;(lex '(lambda (a)
;          (lambda (b)
;        (lambda (c)
;          (lambda (w)
;            (lambda (x)
;          (lambda (y)
;            ((lambda (a)
;               (lambda (b)
;             (lambda (c)
;               (((((a b) c) w) x) y))))
;             (lambda (w)
;               (lambda (x)
;             (lambda (y)
;               (((((a b) c) w) x) y)))))))))))
;       '())
;(lex '(lambda (a)
;          (lambda (b)
;        (lambda (c)
;          (lambda (w)
;            (lambda (x)
;          (lambda (y)
;            ((lambda (a)
;               (lambda (b)
;             (lambda (c)
;               (((((a b) c) w) x) y))))
;             (lambda (w)
;               (lambda (x)
;             (lambda (y)
;               (((((a b) c) w) h) y)))))))))))
;       '())



;;Problem 2, Normal value of
(define value-of
  (λ (e env)
    (match e
      (`,n #:when (number? n) n)
      (`#t #t)
      (`#f #f)
      (`(lambda (,x) ,body)
       #:when (symbol? x)
       (lambda (arg)
         (value-of body
                   (lambda (y)
                     (cond
                       ((eqv? y x) arg)
                       (else (env y))
                       )))
         ))
      (`zero? zero?)
      (`sub1 sub1) 
      (`(if (,pred ,x) ,true-case ,false-case)
       (if ((value-of pred env) (value-of x env)) 
           (value-of true-case env)
           (value-of false-case env))
       )
      (`(let ((,x ,e)) ,body)
       (value-of body
                 (lambda (y)
                   (cond
                     ((eqv? y x) (value-of e env))
                     (else (env y))
                     )))
       )
      (`(* ,e1 ,e2)
       (* (value-of e1 env) (value-of e2 env))
       )
      (`,y #:when (symbol? y) (env y))
       ;;Handling Begin and Set!
      (`(begin2 ,x ,y)
       (value-of x env) (value-of y env))
      (`(set! ,x ,y)
       (set-box! (value-of x env)) ((value-of y env) x))
      (`(,rator ,rand)
       ((value-of rator env) (value-of rand env))))))





;;Value of-fn
(define value-of-fn
  (λ (e env)
    (match e
      (`,n #:when (number? n) n)
      (`#t #t)
      (`#f #f)
      (`(lambda (,x) ,body)
       #:when (symbol? x)
       (make-clos-fn x body env)
       (lambda (arg)
         (value-of body
                   (lambda (y)
                     (cond
                       ((eqv? y x) arg)
                       (else (env y))
                       )))))
      (`zero? zero?)
      (`sub1 sub1)
      (`(if (,pred ,x) ,true-case ,false-case)
       (if ((value-of pred env) (value-of x env)) 
           (value-of true-case env) 
           (value-of false-case env))
       )
      (`(let ((,x ,e)) ,body)
       ((make-clos-fn x body env) (value-of-fn e env)))
      (`(* ,e1 ,e2)
       (* (value-of-fn e1 env) (value-of-fn e2 env)))
      (`,y
       #:when (symbol? y)
       (apply-env-fn env y)) 
      (`(,rator ,rand)
       (apply-clos-fn
        (value-of-fn rator env)
        (value-of-fn rand env))))))

; empty-env-fn
(define empty-env-fn
  (λ ()
    (λ (y)
      (error "unbound variable ~a" y))))

; extend-env-fn
(define extend-env-fn
  (λ (x arg env)
    (λ (y)
      (cond
        ((eqv? y x) arg)
        (else (apply-env-fn env y))))))

; apply-env-fn
(define apply-env-fn
  (λ (env y)
    (env y)))

; apply-clos-fn
(define apply-clos-fn
  (λ (rator rand)
    (rator rand)))

; make-clos-fn
(define make-clos-fn
  (λ (x body env)
    (λ (arg)
      (value-of-fn body (extend-env-fn x arg env)))))


;;TEST CASES

;(value-of
;   '((lambda (x) (if (zero? x)
;                     #t
;                     #f))
;     0)
;   (lambda (y) (error 'value-of "unbound variable ~s" y)))
;
;(value-of
;   '((lambda (x) (if (zero? x)
;                     12
;                     47))
;     0)
;   (lambda (y) (error 'value-of "unbound variable ~s" y)))
;
;(value-of
;   '(let ([y (* 3 4)])
;      ((lambda (x) (* x y)) (sub1 6)))
;   (lambda (y) (error 'value-of "unbound variable ~s" y)))
;(value-of
;   '(let ([x (* 2 3)])
;      (let ([y (sub1 x)])
;        (* x y)))
;   (lambda (y) (error 'value-of "unbound variable ~s" y)))
;
;(value-of
;   '(let ([x (* 2 3)])
;      (let ([x (sub1 x)])
;        (* x x)))
;   (lambda (y) (error 'value-of "unbound variable ~s" y)))
;(value-of
;   '(let ((! (lambda (x) (* x x))))
;      (let ((! (lambda (n)
;                 (if (zero? n) 1 (* n (! (sub1 n)))))))
;        (! 5)))
;   (lambda (y) (error 'value-of "unbound variable ~s" y)))
;
;(value-of
;   '(((lambda (f)
;        (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
;      (lambda (f)
;        (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
;     5)
;   (lambda (y) (error 'value-of "unbound variable ~s" y)))
;
;(value-of-fn
;   '((lambda (x) (if (zero? x)
;                     #t
;                     #f))
;     0)
;   (empty-env-fn))
;
;(value-of-fn
;   '((lambda (x) (if (zero? x)
;                     12
;                     47))
;     0)
;   (empty-env-fn))
;
;(value-of-fn
;   '(let ([y (* 3 4)])
;      ((lambda (x) (* x y)) (sub1 6)))
;   (empty-env-fn))
;(value-of-fn
;   '(let ([x (* 2 3)])
;      (let ([y (sub1 x)])
;        (* x y)))
;   (empty-env-fn))
;(value-of-fn
;   '(let ([x (* 2 3)])
;      (let ([x (sub1 x)])
;        (* x x)))
;   (empty-env-fn))
;
;(value-of-fn
;   '(((lambda (f)
;        (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
;      (lambda (f)
;        (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
;     5)
;   (empty-env-fn))


;;BEGIN AND SET TEST CASES
;;(value-of
;;    '(* (begin2 1 1) 3)
;;    (lambda (y) (error 'value-of "unbound variable ~s" y)))
;
;(value-of
;    '((lambda (a)
;        ((lambda (p)
;           (begin2
;             (p a)
;             a))
;     (lambda (x) (set! x 4))))
;      3)
;     (lambda (y) (error 'value-of "unbound variable ~s" y)))



;;QUESTION 4
(define value-of-lex
  (lambda (exp env)
    (match exp
      [`(const ,expr) expr]
      [`(mult ,x1 ,x2) (* (value-of-lex x1 env) (value-of-lex x2 env))]
      [`(zero ,x) (zero? (value-of-lex x env))]
      (`(sub1 ,body) (sub1 (value-of-lex body env)))
      (`(if ,t ,c ,a) (if (value-of-lex t env) (value-of-lex c env) (value-of-lex a env)))
      (`(var ,num) (apply-env-lex env num))
      (`(lambda ,body) (lambda (a) (value-of-lex body (extend-env-lex a env))))
      (`(,rator ,rand) ((value-of-lex rator env) (value-of-lex rand env))))))

(define empty-env-lex
  (lambda ()
    (lambda (y) (error 'value-of "unbound variable ~s" y))))

(define apply-env-lex
  (lambda (env num)
    (env num)))

(define extend-env-lex
  (lambda (val env)
    (lambda (index)
      (if (eqv? index 0)
          val
          (env (sub1 index))))))

(value-of-lex '((lambda (var 0)) (const 5)) (empty-env-lex))




