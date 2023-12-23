#lang racket

;;Part 1
(require racket/trace)
(define value-of-ds
  (λ (e env)
    (match e
      (`,n #:when (number? n) n)
      (`(+ ,nexpr1 ,nexpr2)
       (+ (value-of-ds nexpr1 env)
          (value-of-ds nexpr2 env)))
      (`(if-null ,l ,null-expr ,not-null-expr)
        (cond
          ((null? (value-of-ds l env))
           (value-of-ds null-expr env))
          (else (value-of-ds not-null-expr env))))
      (`(cons ,car-expr ,cdr-expr)
       (cons (value-of-ds car-expr env)
             (value-of-ds cdr-expr env)))
      (`(car ,l-expr)
       (car (value-of-ds l-expr env)))
      (`(cdr ,l-expr)
       (cdr (value-of-ds l-expr env)))
      ('empty '())
      (`,y #:when (symbol? y) (apply-env-ds env y))
      (`(let ((,x ,v)) ,body)
       (let ((val (value-of-ds v env)))
         (value-of-ds body (extend-env-ds x (value-of-ds v env) env))))
      (`(λ (,x) ,body)
       #:when (symbol? x)
       (make-closure-ds x body env))
      (`(,rator ,rand)
       (apply-closure-ds (value-of-ds rator env) (value-of-ds rand env)
        env)))))


(define extend-env-ds
  (λ (x arg env)
    `(extend-env-ds ,x ,arg ,env)))
(define empty-env-ds
  (λ ()
    `(empty-env-ds)))
(define apply-env-ds
  (λ (env y)
    (match env
      (`(empty-env-ds)
       (error 'val-of "unbound ~a" y))
      (`(extend-env-ds ,x ,arg ,env)
       (cond
         ((eqv? y x) arg)
         (else
          (apply-env-ds env y))))
      (_ (env y)))))
(define apply-closure-ds
  (λ (clos arg env^)
    (match clos
      (`(make-closure-ds ,x ,body ,env)
       (value-of-ds body (extend-env-ds x arg env^))))))
(define make-closure-ds
  (λ (x body env)
    `(make-closure-ds ,x ,body ,env)))

;;(trace value-of-ds)
;(value-of-ds
;   '(let ([x 10])
;      (let ([f (λ  (a) (+ a x))])
;        (let ([x 1])
;          (f 5))))
;   (empty-env-ds))
;(value-of-ds
;   '(let ([map (λ (map)
;                 (λ (f)
;                   (λ (ls)
;                     (if-null ls
;                              empty
;                              (cons
;                               (f (car ls))
;                               (((map map) f)
;                                (cdr ls)))))))])
;      (let ([ls (cons 1 (cons 2 (cons 3 empty)))])
;        (let ([f (λ (a) (cons a ls))])
;          (((map map) f) ls))))
;   (empty-env-ds))


;(value-of-ds
;   '(let ([map (λ (map)
;                 (λ (f)
;                   (λ (ls^)
;                     (if-null ls^
;                              empty
;                              (cons
;                               (f (car ls^))
;                               (((map map) f)
;                                (cdr ls^)))))))])
;      (let ([ls (cons 1 (cons 2 (cons 3 empty)))])
;        (let ([f (λ (a) (cons a ls))])
;          (((map map) f) ls))))
;   (empty-env-ds))



;;Part 2
(define val-of-cbv
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (val-of-cbv n env))]
      [`(sub1 ,n) (sub1 (val-of-cbv n env))]
      [`(* ,n1 ,n2) (* (val-of-cbv n1 env) (val-of-cbv n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbv test env)
                                  (val-of-cbv conseq env)
                                  (val-of-cbv alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbv e1 env) (val-of-cbv e2 env))]
      [`(set! ,x ,expr) #:when (symbol? x) (set-box! (apply-env env x) (val-of-cbv expr env))]
      [`(random ,n) (random (val-of-cbv n env))]
      (`(cons ,car-expr ,cdr-expr)
       (cons (val-of-cbv car-expr env)
             (val-of-cbv cdr-expr env)))
      (`(car ,l-expr)
       (car (val-of-cbv l-expr env)))
      (`(cdr ,l-expr)
       (cdr (val-of-cbv l-expr env)))
      (`(cons^ ,car-expr ,cdr-expr)
       (cons (λ () (val-of-cbv car-expr env)) (λ () (val-of-cbv cdr-expr env))))
       (`(car^ ,l-expr)
        ((car (val-of-cbv l-expr env))))
       (`(cdr^ ,l-expr)
        ((cdr (val-of-cbv l-expr env))))
      (`(empty) '())
      (`(null? ,n) (null? (val-of-cbv n env)))
      [`(add1 ,n) (add1 (val-of-cbr n env))]
      [`(let ((,x ,v)) ,body)
       (let ((val (value-of-ds v env)))
         (value-of-ds body (extend-env-ds x (box (value-of-ds v env)) env)))]
      [`,y #:when (symbol? y) (unbox (apply-env env y))]
      [`(lambda (,x) ,body) (make-closure-cbv x body env)]
      [`(,rator ,rand) (apply-closure (val-of-cbv rator env)
                                      (box (val-of-cbv rand env)))])))

(define val-of-cbr
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (val-of-cbr n env))]
      [`(sub1 ,n) (sub1 (val-of-cbr n env))]
      [`(* ,n1 ,n2) (* (val-of-cbr n1 env) (val-of-cbr n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbr test env)
                                  (val-of-cbr conseq env)
                                  (val-of-cbr alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbr e1 env) (val-of-cbr e2 env))]
      [`(set! ,x ,expr) #:when (symbol? x) (set-box! (apply-env env x) (val-of-cbr expr env))]
      [`(random ,n) (random (val-of-cbr n env))]
      [`(let ((,x ,v)) ,body)
       (let ((val (value-of-ds v env)))
         (value-of-ds body (extend-env-ds x (box (value-of-ds v env)) env)))]
      [`,y #:when (symbol? y) (unbox (apply-env env y))]
      [`(lambda (,x) ,body) (make-closure-cbr x body env)]
      (`(,rator ,x)
       #:when (symbol? x)
       ((val-of-cbr rator env) (apply-env (env x))))
      [`(,rator ,rand) (apply-closure (val-of-cbr rator env)
                                      (box (val-of-cbr rand env)))])))
(define val-of-cbname
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (val-of-cbname n env))]
      [`(sub1 ,n) (sub1 (val-of-cbname n env))]
      [`(* ,n1 ,n2) (* (val-of-cbname n1 env) (val-of-cbname n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbname test env)
                                  (val-of-cbname conseq env)
                                  (val-of-cbname alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbname e1 env) (val-of-cbname e2 env))]
      [`(set! ,x ,expr) #:when (symbol? x) (set-box! (apply-env env x) (val-of-cbname expr env))]
      [`(random ,n) (random (val-of-cbname n env))]
      [`,y #:when (symbol? y) ((unbox (apply-env env y)))]
      [`(lambda (,x) ,body) (make-closure-cbname x body env)]
      (`(,rator ,x)
       #:when (symbol? x)
       ((val-of-cbname rator env) (apply-env env x)))
      [`(,rator ,rand) (apply-closure (val-of-cbname rator env)
                                      (box (λ () (val-of-cbname rand env))))])))
(define val-of-cbneed
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (val-of-cbneed n env))]
      [`(sub1 ,n) (sub1 (val-of-cbneed n env))]
      [`(* ,n1 ,n2) (* (val-of-cbneed n1 env) (val-of-cbneed n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbneed test env)
                                  (val-of-cbneed conseq env)
                                  (val-of-cbneed alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbneed e1 env) (val-of-cbneed e2 env))]
      [`(set! ,x ,expr) #:when (symbol? x) (set-box! (apply-env env x) (val-of-cbneed expr env))]
      [`(random ,n) (random (val-of-cbneed n env))]
      [`,y #:when (symbol? y)
           (let ((b (apply-env env y)))
             (let ((v ((unbox b))))
               (begin
                 (set-box! b (λ () v))
                 v)))]
      [`(lambda (,x) ,body) (make-closure-cbneed x body env)]
      (`(,rator ,x)
       #:when (symbol? x)
       ((val-of-cbneed rator env) (apply-env (env x))))
      [`(,rator ,rand) (apply-closure (val-of-cbneed rator env)
                                      (box (λ () (val-of-cbneed rand env))))])))

(define empty-env
  (λ ()
    (λ (y)
      (error "unbound variable ~a" y))))
(define extend-env
  (λ (x arg env)
    (λ (y)
      (cond
        ((eqv? y x) arg)
        (else (apply-env env y))))))
(define apply-env
  (λ (env y)
    (env y)))
(define apply-closure
  (λ (rator rand)
    (rator rand)))
(define make-closure-cbv
  (λ (x body env)
    (λ (arg)
    (val-of-cbv body (extend-env x arg env)))))
(define make-closure-cbr
  (λ (x body env)
    (λ (arg)
    (val-of-cbr body (extend-env x arg env)))))
(define make-closure-cbname
  (λ (x body env)
    (λ (arg)
    (val-of-cbname body (extend-env x arg env)))))
(define make-closure-cbneed
  (λ (x body env)
    (λ (arg)
    (val-of-cbneed body (extend-env x arg env)))))

(define random-sieve
    '((lambda (n)
        (if (zero? n)
            (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) #t #f) #f) #f) #f) #f) #f)
            (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f #t))))))))
      (random 2)))

(trace val-of-cbname)
(val-of-cbname random-sieve (empty-env))

