#lang racket

;(define idx
;  (λ (y l)
;    (match l
;      (`() (error "not found"))
;      (`(,a . ,d)
;       #:when (eqv? y a)
;       0)
;      (_ (add1 (idx y (cdr l)))))))
;
;;;Lex takes a λ calculus expression along with a lexical environment which is a list, and returns the
;;;corresponding expression using de Bruijn indices
;
;(define lex
;  (λ (e lenv)
;    (match e
;      (`,y #:when (symbol? y)
;           (idx y lenv))
;      (`(λ (,x) ,body)
;       #:when (symbol? x)
;       `(λ ,(lex body (cons x lenv))))
;      (`(,rator ,rand)
;       `(,(lex rator lenv) ,(lex rand lenv))))))
;
(define indices natural?)
;
;(define e1=e2/lex?
;  (λ (e1 e2)
;    (match `(,e1 ,e2)
;      (`(,y1 ,y2)
;       #:when (and (indices y1) (indices y2))
;       (eqv? y1 y2))
;      (`((λ ,body1) (λ ,body2))
;       (e1=e2/lex? body1 body2))
;      (`((,rator1 ,rand1) (,rator2 ,rand2))
;       (and (e1=e2/lex? rator1 rator2)
;       (e1=e2/lex? rand1 rand2))))))
;(e1=e2/lex? (lex '(λ (x) y))
;            (lex '(λ (y) y)))
;(e1=e2/lex? (lex '(λ (x) (λ (y) x)) '())
;            (lex '(λ (y) (λ (x) y)) '()))

;;e := Natural num | (+ e e) | (* e e)

(define arith
  (λ (e)
    (match e
    (`,n
     #:when (natural? n)
     n)
    (`(+ ,e1 ,e2) ;;+ is a symbol, a part of our language
     (+ (arith e1) (arith e2)))
    (`(* ,e1 ,e2) ;;* is a symbol, a part of our language
     (* (arith e1) (arith e2))))))

;;(arith '(+ 2 34))
;;(arith '(+ 2 (* 2 V)))

(define roman-env
  (λ (y)
  (match y
    ('V 5)
    ('I 1)
    ('X 10)
    (_ (error "no such roman numeral in this env")))))

;(define arith-roman
;  (λ (e)
;    (match e
;    (`,n
;     #:when (natural? n)
;     n)
;      (`,y
;       #:when (symbol? y)
;       (roman-env y))
;    (`(+ ,e1 ,e2) ;;+ is a symbol, a part of our language
;     (+ (arith-roman e1) (arith-roman e2)))
;    (`(* ,e1 ,e2) ;;* is a symbol, a part of our language
;     (* (arith-roman e1) (arith-roman e2))))))
;(arith-roman '(+ 2 (* 2 V)))


(define roman-env-ext
  (λ (renv y n)
    (λ (y^)
      (cond
        ((eqv? y^ y) n)
        (else (renv y^))))))


(define roman-env-new (roman-env-ext roman-env 'M 1000))

;(define arith-roman-new
;  (λ (e)
;    (match e
;    (`,n
;     #:when (natural? n)
;     n)
;      (`,y
;       #:when (symbol? y)
;       (roman-env-new y))
;    (`(+ ,e1 ,e2) ;;+ is a symbol, a part of our language
;     (+ (arith-roman-new e1) (arith-roman-new e2)))
;    (`(* ,e1 ,e2) ;;* is a symbol, a part of our language
;     (* (arith-roman-new e1) (arith-roman-new e2))))))

;;(arith-roman-new '(+ M (* V X)))
;;This above implementation is bad because it takes up a global variable?
;
;(define arith-roman
;  (λ (e renv)
;    (match e
;    (`,n
;     #:when (natural? n)
;     n)
;      (`,y
;       #:when (symbol? y)
;       (roman-env y))
;    (`(+ ,e1 ,e2) ;;+ is a symbol, a part of our language
;     (+ (arith-roman e1 renv) (arith-roman e2 renv)))
;    (`(* ,e1 ,e2) ;;* is a symbol, a part of our language
;     (* (arith-roman e1 renv) (arith-roman e2 renv))))))
;(arith-roman '(+ D (* L (+ C 23)))
;             (roman-env-ext
;              (roman-env-ext
;               (roman-env-ext roman-env-new 'D 500) 'L 50) 'C 100))

(define val-of
  (λ (e env)
    (match e
    (`,y
     #:when (symbol? y)
     (env y))
      (`,n
       #:when (natural? n)
       n)
    (`(λ (,x) ,body)
     #:when (symbol? x)
     (λ (arg)
       (val-of body (λ (y)
                      (cond
                        ((eqv? y x) arg)
                        (else (env y)))))))
    (`(,rator ,rand)
     ((val-of rator env) (val-of rand env))))))
(define fn (val-of '(λ (x) (λ (y) 5))
           (λ (y) (error "unbound var ~a" y))))
((val-of '(λ (x) 4)
           (λ (y) (error "unbound var ~a" y)))
1);;Without this number at the end you only get a procedure, but with it you always get 4

((val-of '(λ (x)
             (sub1 x))
           (λ (y)
             (cond
               ((eqv? y 'sub1) sub1)
               (else (error "unbound var ~a" y)))))
11);;This returns 1 minus the number at the end


