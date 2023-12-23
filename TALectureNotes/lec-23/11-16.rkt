#lang racket
(require "monads.rkt")

;; bind-state
;; (-> (State S A)
;;     (-> A (State S B))
;;   (State S B))
;; bind-state changes the pure value (e.g. A above) in the
;; State monad without touching the state S.

;; inj-state
;; (-> A
;;   (State S A))
;; inj-state packages a pure value in a monad.

#|
a is A
ma is (M A)
f is (→ A (M B))
g is (→ A (M B))

The 3 monads laws
- left-id: (bind (inj a) f) ≡ (f a)
- right-id: (bind ma inj) ≡ ma
- associative:
(bind (bind ma f) g) ≡ (bind ma (λ (a) (bind (f a) g)))
|#

(define assv
  (λ (a ls)
    (match ls
      (`() #f)
      (`((,aa . ,ad) . ,d) #:when (eqv? aa a) ad)
      (`((,aa . ,ad) . ,d) (assv a d)))))

;(assv 'c '((a . 2) (b . 3) (c . 4)))
;(assv 'd '((a . 2) (b . 3) (c . 4)))
;(assv 'c '((a . 2) (b . 3) (c . #f)))
;(assv 'd '((a . 2) (b . 3) (c . #f)))

(define assv^
  (λ (a ls)
    (match ls
      (`() (Nothing))
      (`((,aa . ,ad) . ,d) #:when (eqv? aa a) (Just ad))
      (`((,aa . ,ad) . ,d) (assv^ a d)))))

(assv^ 'c '((a . 2) (b . 3) (c . #f)))
(assv^ 'd '((a . 2) (b . 3) (c . #f)))

(define find
  (λ (a ls)
    (match ls
      (`() (Nothing))
      (`(,a^ . ,d) #:when (eqv? a^ a) (Just d))
      (`(,a^ . ,d) (find a d)))))

(find 'a '(x y z))
(find 'a '(x y z a b c))

#;
(define find-3
  (λ (a ls)
    (bind-maybe
      (find a ls)
      (λ (res1)
        (bind-maybe
          (find a res1)
          (λ (res2)
            (bind-maybe
              (find a res2)
              (λ (res3)
                (Just `(,res1 ,res2 ,res3))))))))))

(define find-3
  (λ (a ls)
    (go-on
      (res1 <- (find a ls))
      (res2 <- (find a res1))
      (res3 <- (find a res2))
      (Just `(,res1 ,res2 ,res3)))))

(find-3 'a '(x y z a b c z a h j k a i j a y u v))

(define c-p
  (λ (A B)
    (match B
      (`() '())
      (`(,b) (map (λ (a) `(,a ,b)) A))
      (`(,ba . ,bd)
       (append (c-p A `(,ba)) (c-p A bd))))))

(c-p '(a b c) '(1 2 3 4 5))

#;
(define cartesian-product
  (λ (l1 l2)
    (bind-list
      l1
      (λ (a1)
        (bind-list
          l2
          (λ (a2)
            (inj-list `(,a1 . ,a2))))))))
#;
(cartesian-product '(a b c) '(1 2 3 4 5))


(define cartesian-product
  (λ (l1 l2)
    (go-on
      (a1 <- l1)
      (a2 <- l2)
      (inj-list `(,a1 . ,a2)))))

(cartesian-product '(a b c) '(1 2 3 4 5))

(define valof
  (λ (exp env)
    (match exp
      (`,n #:when (number? n) n)
      (`(+ ,e1 ,e2) (+ (valof e1 env) (valof e2 env)))
      (`,y #:when (symbol? y) (env y))
      (`(λ (,x) ,body)
       (λ (a)
         (valof body (λ (y) (if (eqv? y x) a (env y))))))
      (`(,rator ,rand)
       ((valof rator env) (valof rand env))))))

(define empty-env
  (λ ()
    (λ (y) (error 'empty-env "not found ~a" y))))

(valof '(((λ (x) (λ (y) (+ x y))) 8) 11) (empty-env))

#|
bind-writer
inj-writer
tell
|#

#|
(define valof-writer
  (λ (exp env)
    (match exp
      (`,n #:when (number? n) (inj-writer n))
      (`(+ ,e1 ,e2)
       (bind-writer
        (valof-writer e1 env)
        (λ (n1)
          (bind-writer
           (valof-writer e2 env)
           (λ (n2)
             (let ((result (+ n1 n2)))
               (inj-writer result)))))))
      (`,y
       #:when (symbol? y)
       (inj-writer (env y)))
      (`(λ (,x) ,body)
       (inj-writer
         (λ (a)
           (valof-writer body (λ (y) (if (eqv? y x) a (env y)))))))
      (`(,rator ,rand)
       (bind-writer
        (valof-writer rator env)
        (λ (val-rator)
          (bind-writer
           (valof-writer rand env)
           (λ (val-rand)
             (bind-writer
              (tell `((rator rand) (,val-rator ,val-rand)))
              (λ (_)
                (val-rator val-rand)))))))))))

(run-writer (valof-writer '(((λ (x) (λ (y) (+ x y))) 8) 11) (empty-env)))
|#

(define valof-writer
  (λ (exp env)
    (match exp
      (`,n #:when (number? n) (inj-writer n))
      (`(+ ,e1 ,e2)
       (go-on
         (n1 <- (valof-writer e1 env))
         (n2 <- (valof-writer e2 env))
         (tell `((+ e1 e2) (+ ,n1 ,n2)))
         ;; instead of using assign, we can write
         ;;   (inj-writer (+ n1 n2))
         ;; directly
         (assign (result <- (+ n1 n2)))
         (inj-writer result)))
      (`,y
       #:when (symbol? y)
       (inj-writer (env y)))
      (`(λ (,x) ,body)
       (inj-writer
         (λ (a)
           (valof-writer body (λ (y) (if (eqv? y x) a (env y)))))))
      (`(,rator ,rand)
       (go-on
         (val-rator <- (valof-writer rator env))
         (val-rand <- (valof-writer rand env))
         (tell `((rator rand) (,val-rator ,val-rand)))
         (val-rator val-rand))))))

(run-writer (valof-writer '(((λ (x) (λ (y) (+ x y))) 8) 11) (empty-env)))

#|
bind-k
inj-k
callcc
|#
#;
(define valof-k
  (λ (exp env)
    (match exp
      (`,n #:when (number? n) (inj-k n))
      (`(+ ,e1 ,e2)
       (bind-k
        (valof-k e1 env)
        (λ (n1)
          (bind-k
           (valof-k e2 env)
           (λ (n2)
             (let ((result (+ n1 n2)))
               (inj-k result)))))))
      (`(letcc ,k ,body)
       #:when (symbol? k)
       (callcc
        (λ (k^)
          (valof-k body
                   (λ (y) (if (eqv? y k) k^ (env y)))))))
      (`,y
       #:when (symbol? y)
       (inj-k (env y)))
      (`(λ (,x) ,body)
       (inj-k
        (λ (a)
          (valof-k body (λ (y) (if (eqv? y x) a (env y)))))))
      (`(,rator ,rand)
       (bind-k
        (valof-k rator env)
        (λ (val-rator)
          (bind-k
           (valof-k rand env)
           (λ (val-rand)
             (val-rator val-rand)))))))))



(define valof-k
  (λ (exp env)
    (match exp
      (`,n #:when (number? n) (inj-k n))
      (`(+ ,e1 ,e2)
       (go-on
         (n1 <- (valof-k e1 env))
         (n2 <- (valof-k e2 env))
         (assign (result <- (+ n1 n2)))
         (inj-k result)))
      (`(letcc ,k ,body)
       #:when (symbol? k)
       (callcc
        (λ (k^)
          (valof-k body
                   (λ (y) (if (eqv? y k) k^ (env y)))))))
      (`,y
       #:when (symbol? y)
       (inj-k (env y)))
      (`(λ (,x) ,body)
       (inj-k
         (λ (a)
           (valof-k body (λ (y) (if (eqv? y x) a (env y)))))))
      (`(,rator ,rand)
       (go-on
         (val-rator <- (valof-k rator env))
         (val-rand <- (valof-k rand env))
         (val-rator val-rand))))))

((run-k (valof-k '(((λ (x) (λ (y) (+ x y))) 8) 11) (empty-env))) (λ (v) v))


((run-k (valof-k '(((λ (x) (λ (y) (+ x y)))
                    (letcc k (+ 40 (k (+ 1 1)))))
                   3)
                 (empty-env)))
 (λ (v) v))

(let ((a 5)
      (f (λ (a) (inj-state (add1 a))))
      (bind bind-state)
      (inj inj-state))
  (equal? ((run-state (bind (inj a) f)) 0)
          ((run-state (f a)) 0)))

;; To actually prove it, the following is a way to
;; derive left-id law for the state monad. Here the type (M B)
;; corresponds to the State monad which contains a pure
;; value of type B.
#|
(bind-state (inj-state a) f) ≡ (f a)

; From the definition of bind-state =>

((λ (ma f)
   (State (λ ((s : S))
            (match ((run-state ma) s)
              (`(,a . ,s) ((run-state (f a)) s))))))
 (inj-state a) f)
≡
(f a)

; β reduction =>

(State (λ ((s : S))
         (match ((run-state (inj-state a)) s)
           (`(,a . ,s) ((run-state (f a)) s)))))
≡
(f a)

; from definition of run-state =>

(State (λ ((s : S))
         (match ((State-run-state (inj-state a)) s)
           (`(,a . ,s) ((State-run-state (f a)) s)))))
≡
(f a)

; from definition of inj-state =>

(State (λ ((s : S))
         (match ((State-run-state
                  ((λ (a)
                     (State (λ ((s : S)) (cons a s))))
                   a))
                 s)
           (`(,a . ,s) ((State-run-state (f a)) s)))))
≡
(f a)

; β reduction =>

(State (λ ((s : S))
         (match ((State-run-state
                  (State (λ ((s : S)) (cons a s))))
                 s)
           (`(,a . ,s) ((State-run-state (f a)) s)))))
≡
(f a)

; State-run-state is struct field accessor which returns
; the λ stored inside the State struct=>

(State (λ ((s : S))
         (match ((λ ((s : S)) (cons a s))
                 s)
           (`(,a . ,s) ((State-run-state (f a)) s)))))
≡
(f a)

; β reduction =>

(State (λ ((s : S))
         (match (cons a s)
           (`(,a . ,s) ((State-run-state (f a)) s)))))
≡
(f a)

; result of the pattern match evaluation =>

(State (λ ((s : S))
         ((State-run-state (f a)) s)))
≡
(f a)

; If f is of type (→ A (M B)) and a is of type A then,
; (f a) is of type (M B).
; (State (λ (s) (cons b s))) is a generic value
; of type (M B) given that b is of type B =>

(State (λ ((s : S))
         ((State-run-state (State (λ (s) (cons b s)))) s)))
≡
(State (λ (s) (cons b s)))

; from the definition of State-run-state
;; Struct field accessor  =>

(State (λ ((s : S))
         ((λ (s) (cons b s)) s)))
≡
(State (λ (s) (cons b s)))

; β reduction in body of (λ ((s : S)) ...)
; (s : S) is just a type annotation and can be
; rewritten as just s =>

(State (λ (s)
         (cons b s)))
≡
(State (λ (s)
         (cons b s)))
|#

;; (: find (-> A (Listof A) (Maybe A)))
