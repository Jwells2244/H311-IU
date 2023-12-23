#lang racket


(define e₁=e₂? ;;Same e1=e2 as in the last lecture
  (λ (e₁ e₂)
    (match `(,e₁ ,e₂)
      (`(,y₁ ,y₂)
       #:when (and (symbol? y₁) (symbol? y₂))
       (eqv? y₁ y₂))
      (`((λ (,x₁) ,body₁) (λ (,x₂) ,body₂))
       #:when (and (symbol? x₁) (symbol? x₂))
       (and (eqv? x₁ x₂) (e₁=e₂? body₁ body₂)))
      (`((,rator₁ ,rand₁) (,rator₂ ,rand₂))
       (and (e₁=e₂? rator₁ rator₂) (e₁=e₂? rand₁ rand₂))))))

 ;;(e₁=e₂? '(λ (x) x) '(λ (y) y))

(define free?
  (λ (var expr)
    (match expr
      (`,y
       #:when (symbol? y)
       (eqv? var y))
      (`(λ (,x) ,body)
       #:when (symbol? x)
       (and (free? var body)
            (not (eqv? x var))))
      (`(,rator ,rand)
       (or (free? var rator) (free? var rand))))))

;; (free? 'x '(x (e y)))
(free? 'x '(λ (x) (λ (y) x)))
(free? 'z '(λ (y) (λ (x) (z (w x)))))
(free? 'z '(λ (y) (λ (x) (w x))))

(define bound?
  (λ (var expr)
    (match expr
      (`,y
       #:when (symbol? y)
       #f)
      (`(λ (,x) ,body)
       #:when (symbol? x)
       (or (bound? var body)
           (and (eqv? x var)            ;← checks if it has a home
                (free? var body))))     ;← checks if it "occurs free"
      (`(,rator ,rand)
       (or (bound? var rator) (bound? var rand))))))

;; (bound? 'x '(λ (x) (λ (y) (x y))))
;; (bound? 'x '(λ (x) (λ (y) y)))
;; (bound? 'x '(λ (x) x))
;; (bound? 'x 'x)
;; (bound? 'x '((λ (y) y) (λ (z) (w x))))
;; (bound? 'x '((λ (y) x) (λ (x) x)))
(free? 'y '(λ (x)
               (λ (y)
                 (λ (z)
                   (λ (w)
                     (λ (x)
                       (λ (y)
                         (w ((z x) y)))))))));;The y is bound here
;; ((((λ (x)
;;      (λ (y)
;;        (λ (z)
;;          (λ (w)
;;            (λ (x)
;;              (λ (y)
;;                (w ((z x) y))))))))
;;    12)
;;   13)
;;  15)

;; '(λ (x)
;;    (λ (y)
;;      (λ (w)
;;        (λ (z)
;;          (λ (x)
;;            (λ (y)
;;              (z ((w x) y))))))))

(define index
  (λ (var ls)
    (match ls
      (`() (error "no such var in ls."))
      (`(,a . ,_)
       #:when (eqv? a var)
       0)
      (`(,_ . ,d)
       (add1 (index var d))))))

(define global-formals '(y x w z y x))
(define db-idx-x (index 'x global-formals))
(define db-idx-y (index 'y global-formals))
(define db-idx-z (index 'z global-formals))
(define db-idx-w (index 'w global-formals))

`(λ
   (λ
     (λ
       (λ
         (λ
           (λ
             (,db-idx-z
              ((,db-idx-w ,db-idx-x)
               ,db-idx-y))))))))

(define lex
  (λ (e ls)
    (match e
      (`,y
       #:when (symbol? y)
       (index y ls))
      (`(λ (,x) ,body)
       #:when (symbol? x)
       `(λ ,(lex body (cons x ls))))
      (`(,rator ,rand)
       `(,(lex rator ls) ,(lex rand ls))))))

;; (lex '(x e) '(e x))

(lex '(λ (x)
        (λ (y)
          (λ (w)
            (λ (z)
              (λ (x)
                (λ (y)
                  (z ((w x) y))))))))
     '())
