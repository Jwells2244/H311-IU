#lang racket

;(define e1=e2?
;  (λ (e1 e2)
;    (match `(,e1 ,e2)
;      (`(,y1 ,y2)
;       #:when (and (symbol? y1) (symbol? y2))
;       (eqv? y1 y2))
;      (`((λ (,x1) ,body1))))))
;


;;Free variables are when a lambda is defining a variable with a home, but another variable
;;exists, it is called a free variable because it does not have the lambda as its home

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

;;(free? 'x '(x (e y)))
;;(free? 'x '(λ (x) (λ (y) x)))
;;(free? 'z '(λ (y) (λ (x) (z (w x)))))
;;(free? 'z '(λ (y) (λ (x) (w x))))

;;Variables are bound if they have a home.

(define bound?
  (λ (var expr)
    (match expr
      (`,y
       #:when (symbol? y)
       #f)
      (`(λ (,x) ,body)
       #:when (symbol? x)
       (or (bound? var body)
           (and (eqv? x var)    ; Checks if it has a home
                (free? var body))))  ; Checks if it "occurs" free
      (`(,rator ,rand)
       (or (bound? var rator) (bound? var rand))))))
;;(bound? 'x '(λ (x) (λ (y) (x y))))
;;(free? 'x '((λ (y) y) (λ (z) (w x))))

(bound? 'x '((λ (y) x) (λ (x) x)))
(bound? 'x '(λ (z)
              (λ (w)
                (λ (z)
                  (λ (w)
                    (λ (x)
                      (λ (y)
                        (w ((z x) y)))))))))
(bound? 'z '(λ (z)
              (λ (w)
                (λ (z)
                  (λ (w)
                    (λ (x)
                      (λ (y)
                        (w ((z x) y)))))))))
(bound? 'w '(λ (z)
              (λ (w)
                (λ (z)
                  (λ (w)
                    (λ (x)
                      (λ (y)
                        (w ((z x) y)))))))))
(bound? 'y '(λ (z) ;;5
              (λ (w) ;;4
                (λ (z) ;;3
                  (λ (w) ;;2
                    (λ (x) ;;1
                      (λ (y) ;;0
                        (w ((z x) y))))))))) ;;The ys home is the λ right above it, bound by the closest lambda, so we start
;;numbering from that lambda

;;Formals are lambda variables, I guess like formal definitions?
;;Numbered
(λ (z) ;;5
              (λ (w) ;;4
                (λ (z) ;;3
                  (λ (w) ;;2
                    (λ (x) ;;1
                      (λ (y) ;;0
                        (2 ((3 1) 0))))))))
(define index
  (λ (var ls)
    (match ls
      ; If List is Empty
      (`() (error "No Such Var in List"))

      ; If "a" (car) of List = Var, Return 0
      ; "_" means ignored
      (`(,a . ,_)
       #:when (eqv? a var)
       0)

      ; Otherwise, step further into list
      (`(,_ . ,d)
       (add1 (index var d)))
      )))

(define global-formals '(y x w z y x))
(define db-index-x (index 'x global-formals))
(define db-index-y (index 'y global-formals))
(define db-index-z (index 'z global-formals))
(define db-index-w (index 'w global-formals))

;`(λ
;     (λ
;         (λ
;             (λ
;                 (λ
;                     (λ
;                         (,db-idx-z
;                          (,db-idx-w ,db-idx-x)
;                           ,db-idx-y)))))))
(define lex ;;the ,body is the definition where it means the line above
  (λ (e ls)
    (match e
    (`,y
     #:when (symbol? y)
     (index y ls))
      (`(λ (,x) ,body)
       #:when (symbol? x)
       `(λ ,(lex body (cons x ls))))
      (`(,rator ,rand)
    `((lex rator ls) ,(lex rand ls))))))

(lex '(x e) '(e x))
(lex '(λ (z)
              (λ (w)
                (λ (z)
                  (λ (w)
                    (λ (x)
                      (λ (y)
                        (w ((z x) y))))))))
     '())
                             
