#lang racket
(require racket/trace)

;; We will be writing an interpreter that evaluates
;; the following Racket expression
(letrec ((map (λ (f)
                (λ (ls)
                  (cond
                    ((null? ls) '())
                    (else
                     (cons
                      (f (car ls))
                      ((map f) (cdr ls))))))))
         (ls '(1 2 3))
         (f (λ (a) (cons a ls))))
  ((map f) ls))

;;Poor mans Y-combinator, just applies the argument to itself.
;; Before that let us explore recursion.
;; This is named as poor person's Y-combinator, a.k.a. poor
;; man's Y-Combinator.
(define app-self
  (λ (x)
    (x x)))

(define proc (app-self (λ (x) x)))
;; (app-self (λ (x) x))
;; ↦ ((λ (x) (x x)) (λ (x) x))
;; ↦ ((λ (x) x) (λ (x) x))
;; ↦ (λ (x) x)
;; Try applying other functions to app-self

;; Ω combinator
#;
(define Ω (app-self app-self))
;; Ω runs forever because:
;; Ω
;; ↦ (app-self app-self)
;; ↦ ((λ (x) (x x)) app-self)
;; ↦ (app-self app-self)
;; ↦ Ω
;; ↦ (app-self app-self) ↦ ...

;; (letrec ((map (λ (f)
;;                 (λ (ls)
;;                   (cond
;;                     ((null? ls) '())
;;                     (else
;;                      (cons
;;                       (f (car ls))
;;                       ((map f) (cdr ls))))))))
;;          (ls '(1 2 3))
;;          (f (λ (a) (cons a ls))))
;;   ((map f) ls))
;We can now use app-self to achieve recursion without using
;letrec.
;; We now use app-self to achieve recursion
;; without using letrec. We will cover evaluating
;; letrec in our interpreter in a future lecture.
(let ((map (λ (map)
             (λ (f)
               (λ (ls)
                 (cond
                   ((null? ls) '())
                   (else
                    (cons
                     (f (car ls))
                     (((app-self map) f)
                      (cdr ls)))))))))
      (ls '(1 2 3)))
  (let ((f (λ (a) (cons a ls))))
    (((app-self map) f) ls)))


;; We transform the above expression so that we don't
;; use quotes and so that we use let expressions that
;; bind a single variable.
(let ((map (λ (map)
             (λ (f)
               (λ (ls)
                 (cond
                   ((null? ls) '())
                   (else (cons
                          (f (car ls))
                          (((app-self map) f)
                           (cdr ls))))))))))
  (let ((ls (cons 1 (cons 2 (cons 3 empty)))))
    (let ((f (λ (a) (cons a ls))))
      (((app-self map) f) ls))))
;Modify this expression so that we dont use quotes and so that we use let expressions that bing a single variable.

;; We now add match lines to our representation dependent
;; interpreter from last class so that we can interpret the
;; above example expression. Note that we are using the if-null
;; syntactic form in our interpreter instead of cond.
(define value-of
  (λ (e env)
    (match e
      (`,n #:when (number? n) n)
      (`(+ ,nexpr1 ,nexpr2)
       (+ (value-of nexpr1 env)
          (value-of nexpr2 env)))
      (`(if-null ,l ,null-expr ,not-null-expr)
        (cond
          ((null? (value-of l env))
           (value-of null-expr env))
          (else (value-of not-null-expr env))))
      (`(cons ,car-expr ,cdr-expr)
       (cons (value-of car-expr env)
             (value-of cdr-expr env)))
      (`(car ,l-expr)
       (car (value-of l-expr env)))
      (`(cdr ,l-expr)
       (cdr (value-of l-expr env)))
      (`empty '())
      ; Lookup the symbol y in environment
      (`,y #:when (symbol? y) (env y))
      (`(let ((,x ,v)) ,body)
       ; what's wrong with this definition?
       (value-of `((λ (,x) ,body) ,v) env)
       #;
       (let ((val (value-of v env)))
         (value-of body (λ (y)
                          (cond
                            ((eqv? y x) val)
                            (else (env y))))))
       )
      ; Return function
      (`(λ (,x) ,body)
       #:when (symbol? x)
       (λ (arg)
         ; We also need to extend the environment
         (value-of body (λ (y)
                          (cond
                            ((eqv? y x) arg)
                            (else (env y)))))))
      ; Application should apply and of course,
      ; natural recursion
      (`(,rator ,rand)
       ((value-of rator env) (value-of rand env))))))

(value-of '(let ((w 1))
             (let ((z 2))
               (+ 5 (+ w z))))
          (λ (y) (error 'value-of "not found ~a" y)))
(value-of
 '(let ((map (λ (map)
               (λ (f)
                 (λ (ls)
                   (if-null ls
                            empty
                            (cons
                             (f (car ls))
                             (((map map) f)
                              (cdr ls)))))))))
    (let ((ls (cons 1 (cons 2 (cons 3 empty)))))
      (let ((f (λ (a) (cons a ls))))
        (((map map) f) ls))))
 (λ (y) (error 'value-of "not found ~a" y)))

;;Value-of, like Racket in general, uses a lexical scope, meaning that free variables in the λ term refer to the
;environment present during creation of the λ term.
;; value-of uses a lexical scope meaning that free variables
;; in the λ term refer to the environment present during creation
;; of the λ term.


;; This example gives us 11 instead of 2 because the x in the
;; body of the λ refers to the environment where x is bound
;; by the let above it. The environment when x is applied to x
;; refers to the x that has the value 1 and this x is ignored.
;;This expression gives us 11, instead of 2, because the x in the body is bound by the let above it
;The enviroment where x is applied to x, refers to the x that has the value of 1 and this x is ignored.
;Therefore, the x that we work with has the value of 10 in the let, and the a has the value of 1 I think
(value-of ;Bad explanantion^. When the f gets defined, the x inside the lambda is 10. When the f gets called, with a new x equalling 1
 '(let ((x 10)) ;the a gets replaced by that x, and the original x=10 stays the same. This is lexical scoping. Free variables in the λ term refer to the environment
    (let ((f (λ (a) (+ a x)))) ;f = λ (a) (+ a x) present during the creation of the λ term.
      (let ((x 1)) ; tempx = 1
        (f x)))) ;1 gets passed into the x spot, whereas the 10 is what the a exists as before, so we get 11
 (λ (y) (error 'value-of "not found ~a" y)))

;; Evaluating the same expression in Racket gives us the same
;; result meaning that Racket is also lexically scoped.
;;This is just how racket evaluates it as well, meaning that Racket is lexically scoped as well
(let ((x 10))
  (let ((f (λ  (a) (+ a x))))
    (let ((x 1))
      (f x))))


;; If we want to use the environment available when the λ
;; is applied instead of when the λ is created, we use a
;; dynamically-scoped interpreter. Here is one that is a
;; modified version of the above lexically-scoped interpreter:
;;Here is the dynamically scoped interpreter, where the symbol can be looked up in the environment.
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
      ; Lookup the symbol y in environment
      (`,y #:when (symbol? y) (env y))
      (`(let ((,x ,v)) ,body)
       (let ((val (value-of-ds v env)))
         (value-of-ds body (λ (y)
                             (cond
                               ((eqv? y x) val)
                               (else (env y)))))))
      ; Return function
      (`(λ (,x) ,body)
       #:when (symbol? x)
       ;; Replacing the formal env with env^ turns this into
       ;; a lexically scoped interpreter ;Adding the env^ turns this into a lexically scoped interpreter, like the one on practice the exam. However, if you 
       (λ (arg env) ;add this env^, you would need to replace the one bound by this variable in order to not get an error, like the practice exam.
         ; We also need to extend the environment but the 
         ; env is not bound from the usual one
         (value-of-ds body (λ (y)
                             (cond
                               ((eqv? y x) arg)
                               (else (env y)))))))
      (`(,rator ,rand)
       ((value-of-ds rator env)
        (value-of-ds rand env)
        env)))))
;Dynamically scoped interpreters basically redefine the environment when going into the return clause I think

;; This interpreter gives us the result when the environment
;; at the time of evaluating (f x) is used to evaluate the
;; body of the λ. This environment has x bound to 1.
(value-of-ds
 '(let ((x 10))
    (let ((f (λ  (a) (+ a x))))
      (let ((x 1))
        (f x)))) 
 (λ (y) (error 'value-of "not found ~a" y)))
;Now, with the dynamic scoping, this expression returns 2, where x has now been bound to 1 in the environment, and so passing the x as the a value makes it (+ 1 x),
;And we know that x equals 1 from the passing, so instead the expression is (+ 1 1), instead of (+ 1 10) in the lexically scoped evaluation of this expression.


;; Running the program that we have been discussing at the top
;; gives us a different result now. Try using tagged lists
;; as environments in value-of-ds and look at the trace to
;; understand what is going on.
;In this dynamic scope environment, we get ((1 1 2 3) (2 2 3) (3 3))
(value-of-ds
 '(let ((map (λ (map)
               (λ (f)
                 (λ (ls)
                   (if-null ls
                            empty
                            (cons
                             (f (car ls))
                             (((map map) f)
                              (cdr ls)))))))))
    (let ((ls (cons 1 (cons 2 (cons 3 empty)))))
      (let ((f (λ (a) (cons a ls))))
        (((map map) f) ls))))
 (λ (y) (error 'value-of "not found ~a" y)))


;; renaming ls to ls^ gives us the result that we got from
;; the lexically scoped interpreter
;In this lexical scoped environment, after we rename the ls to ls^, we get the intended result of:
;((1 1 2 3) (2 1 2 3) (3 1 2 3)). This is because the dynamic scoping uses what ls gets assigned to after the first let, in the first let when ls is called.
;By changing it, we are distinguishing it from what gets changed afterwords, basically making it lexically scoped again because there is no variable overlap
(value-of-ds
 '(let ((map (λ (map)
               (λ (f)
                 (λ (ls^)
                   (if-null ls^
                            empty
                            (cons
                             (f (car ls^))
                             (((map map) f)
                              (cdr ls^)))))))))
    (let ((ls (cons 1 (cons 2 (cons 3 empty)))))
      (let ((f (λ (a) (cons a ls))))
        (((map map) f) ls))))
 (λ (y) (error 'value-of "not found ~a" y)))

;; Dynamic scope is not bad, not terrible it is horrific:
;; https://en.wikipedia.org/wiki/Soyuz-U#:~:text=A%20Soyuz%2DU%20mission%20failed,had%20failed%20to%20reach%20orbit.
