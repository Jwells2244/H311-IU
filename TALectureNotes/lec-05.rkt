#lang racket
(require racket/trace)

;; idx was defined in the last lecture
(define idx
  (λ (y l)
    (match l
      (`()
       (error "not found"))
      (`(,a . ,d)
       #:when (eqv? y a)
       0)
      (_ (add1 (idx y (cdr l)))))))

;; lex takes a λ calculus expression along with
;; a lexical environment which is a list, and
;; returns the corresponding expression
;; using de Bruijn indices.
(define lex
  (λ (e lenv)
    (match e
      (`,y
       #:when (symbol? y)
       (idx y lenv))
      (`(λ (,x) ,b)
       #:when (symbol? x)
       `(λ ,(lex b (cons x lenv))))
      (`(,rator ,rand)
       `(,(lex rator lenv) ,(lex rand lenv))))))

(lex '(λ (x)
        (λ (y)
          (λ (z)
            (λ (y)
              (λ (w)
                (λ (x)
                  (λ (y)
                    ((((x y) z) ((y w) x)) y))))))))
     '())


;; Notice that the indices for the two occurrences
;; of z are different
(lex '(λ (x)
        (λ (y)
          (λ (z)
            (λ (y)
              (λ (w)
                ((λ (x)
                   (λ (y)
                     ((x y) z)))
                 (λ (z)
                   (w z))))))))
     '())

;; lex can be considered as a compiler that compiles
;; expressions in the λ calculus language to
;; expressions that use de Bruijn indices with the
;; following grammar:
;; e ::= Indicie | (λ e) | (e e)

;; Remember e1=e2? Let us define the same function
;; but for the de Bruijn index based λ calculus
;; expressions instead.

(define indicie? natural?)

(define e1=e2/lex?
  (λ (e1  e2)
    (match `(,e1 ,e2)
      (`(,y1 ,y2)
       #:when (and (indicie? y1) (indicie? y2))
       (eqv? y1 y2))
      (`((λ ,b1) (λ ,b2))
       (e1=e2/lex? b1 b2))
      (`((,rator1 ,rand1) (,rator2 ,rand2))
       (and (e1=e2/lex? rator1 rator2)
            (e1=e2/lex? rand1 rand2))))))

;; e1=e2? would give us false for the examples
;; below, even though the variable names
;; shouldn't matter. However, checking for
;; equality after using lex gives us the
;; answer that we desire.

(e1=e2/lex?
 (lex '(λ (x) x) '())
 (lex '(λ (y) y) '()))

(e1=e2/lex?
 (lex '(λ (x) (λ (y) x)) '())
 (lex '(λ (y) (λ (x) y)) '()))

;; Now, lets forget about indicies and look at something
;; completely different.

;; arith evaluates arithmetic expressions with
;; the following grammar:
;; e ::= Natural | (+ e e) | (* e e)

;; In other words, arith is an interpreter that
;; takes an expression e with natural numbers
;; and two arithmetic functions: *, +, and
;; calculates the result.
;; When we write r-function we mean a racket function.
(define arith
  (λ (e)
    (match e
      (`,n #:when (natural? n) n)
      (`(+ ,e1 ,e2)
       ; + is a symbol in the grammar of arith.
       (+ (arith e1) (arith e2)))
       ; + is a r-function.
      (`(* ,e1 ,e2)
       ; * is a symbol in the grammar of arith.
       (* (arith e1) (arith e2))
       ; * is a r-function.
       ))))

(arith '(+ (* 5 0) 3))
(arith '(* (* 5 10) (+ 10 3)))

;; Now, let us start using Roman numerals
;; in our arithmetic expressions.

;; A Roman environment contains associations from Roman
;; numeral symbols to their corresponding
;; natural numbers.

(define roman-env
  (lambda (y)
    (match y
      ('I 1)
      ('V 5)
      ('X 10)
      (_ (error "not roman")))))

;; We can now modify our interpreter arith
;; to use roman-env such that it can evaluate
;; expressions with the following grammar:
;; e := I | V | X | Natural | (+ e e) | (* e e)
;; In this new arith-roman, one can use both
;; roman numerals and natural numbers at the same
;; time.
(define arith-roman
  (λ (e)
    (match e
      (`,y #:when (symbol? y) (roman-env y))
      (`,n #:when (natural? n) n)
      (`(+ ,e1 ,e2)
       (+ (arith-roman e1) (arith-roman e2)))
      (`(* ,e1 ,e2)
       (* (arith-roman e1) (arith-roman e2))))))

(arith-roman '(+ (* V 0) 3))
(arith-roman '(+ V I))
(arith-roman '(+ X I))

;; How do we add new Roman numeral symbols like
;; L and C to roman-env ?

;; We could add new match lines to the original
;; definition of roman-env, but we will be defining
;; a new function that extends an old Roman
;; environment with new Roman numeral symbols.

;; roman-env-ext extends a Roman environment renv
;; by adding a new association from a Roman numeral
;; symbol y to a natural number n. This is similar
;; to the stretch function from assignment 2.

(define roman-env-ext
  (λ (renv y n)
    (λ (y^)
      (cond
        ((eqv? y^ y) n)
        (else (renv y^))))))
;; roman-env-ext contains all associations from
;; roman-env and the new association from given y
;; to given n.

;; roman-env-new is a Roman environment that
;; contains all the associations of roman-env along
;; with the new association from L to 50.
(define roman-env-new
  (roman-env-ext roman-env 'L 50))

;; Now we can use the new environment by creating
;; a new arithmetic interpreter.
(define arith-roman^
  (λ (e)
    (match e
      (`,y #:when (symbol? y)
           (roman-env-new y))
      (`,n #:when (natural? n) n)
      (`(+ ,e1 ,e2)
       (+ (arith-roman^ e1) (arith-roman^ e2)))
      (`(* ,e1 ,e2)
       (* (arith-roman^ e1) (arith-roman^ e2))))))

;; using roman-env-new in arith-roman^ we can
;; evaluate expressions containing L.
(arith-roman^ '(+ X L))

;; It is cumbersome, however, to create a new arithmetic
;; interpreter for every new Roman environment,
;; so we will add a formal renv that represents
;; the new Roman environment.

(define arith-roman^^
  (λ (e renv)
    (match e
      (`,y #:when (symbol? y) (renv y))
      (`,n #:when (natural? n) n)
      (`(+ ,e1 ,e2)
       (+ (arith-roman^^ e1 renv)
          (arith-roman^^ e2 renv)))
      (`(* ,e1 ,e2)
       (* (arith-roman^^ e1 renv)
          (arith-roman^^ e2 renv))))))


(arith-roman^^ '(* M (+ C L))
               (roman-env-ext
                (roman-env-ext
                 roman-env-new
                 'C 100)
                'M 1000))

; Food for thought:
; What's the difference between the lexical environment
; that we used in lex and this Roman environment?
; The lex environment was "compile-time", and the
; Roman environment is "run-time".

;; Now, using the same ideas lets try to write an
;; interpreter for the λ Calculus.
;; val-of is an interpreter that evaluates λ calculus
;; expressions with natural numbers.

;; (define val-of
;;   (λ (e env)
;;     (match e
;;       (`,y #:when (symbol? y)
;;            (env y))
;;       (`(λ (,x) ,body)
;;        #:when (symbol? x)
;;        (λ (arg)
;;          (...)))
;;       (`(,rator ,rand)
;;        (val-of rator env) (val-of rand env))))))


;; (define val-of
;;   (λ (e env)
;;     (match e
;;       (`,y #:when (symbol? y)
;;            (env y))
;;       (`(λ (,x) ,body)
;;        #:when (symbol? x)
;;        (λ (arg)
;;          (val-of body (...))))
;;       (`(,rator ,rand)
;;        (val-of rator env) (val-of rand env))))))

;; (define val-of
;;   (λ (e env)
;;     (match e
;;       (`,y #:when (symbol? y)
;;            (env y))
;;       (`(λ (,x) ,body)
;;        #:when (symbol? x)
;;        (λ (arg)
;;          (val-of body (λ (y)
;;                         (cond
;;                           ((eqv? y x) arg)
;;                           (else (env y)))))))
;;       (`(,rator ,rand)
;;        (val-of rator env) (val-of rand env)))))

;; (define val-of
;;   (λ (e env)
;;     (match e
;;       (`,y #:when (symbol? y)
;;            (env y))
;;       (`(λ (,x) ,body)
;;        #:when (symbol? x)
;;        (λ (arg)
;;          (val-of body (λ (y)
;;                         (cond
;;                           ((eqv? y x) arg)
;;                           (else (env y)))))))
;;       (`(,rator ,rand)
;;        ((val-of rator env) (val-of rand env))))))

;; To test the following we need a match case for
;; numbers, so,


(define val-of
  (λ (e env)
    (match e
      (`,n #:when (natural? n)
           n)
      (`,y #:when (symbol? y)
           (env y))
      (`(λ (,x) ,body)
       #:when (symbol? x)
       (λ (arg)
         (val-of body (λ (y)
                        (cond
                          ((eqv? y x) arg)
                          (else (env y)))))))
      (`(,rator ,rand)
       ((val-of rator env) (val-of rand env))))))

(val-of '((λ (x) x) 5)
        ;; the lambda below is the empty environment
        ;; that will get applied when we try to find
        ;; an association for an unbound variable.
        (λ (y)
          (error 'val-of "unbound variable ~a" y)))

(val-of '((λ (x) (λ (y) x)) 5)
        (λ (y)
          (error 'val-of "unbound variable ~a" y)))

;; λ expressions after going through val-of evaluate to a Racket function
(define y (val-of '(λ (y) 2)
                  (λ (y)
                    (error 'val-of "unbound variable ~a" y))))
;; We can then apply the evaluated Racket
;; function in Racket
(y 3)

(val-of '(((λ (x) (λ (y) x)) 5) 3)
        (λ (y)
          (error 'val-of "unbound variable ~a" y)))

;; (val-of '((λ (x) z) 3)
;;         (λ (y)
;;           (error 'val-of "unbound variable ~a" y)))

(define id
  (val-of '((λ (x) x)
            (λ (x) x))
          (λ (y)
            (error 'val-of "unbound variable ~a" y))))
(id 100)

(val-of '(((λ (x) x)
           (λ (x) x))
          100)
        (λ (y)
          (error 'val-of "unbound variable ~a" y)))

;; This won't work because we don't have sub1
;; in our interpreter.

;; (val-of '(sub1 5)
;;          (empty-env))

;; But, since it is a function with one argument
;; we can add it to our interpreter by extending
;; the environment.
(val-of '(sub1 5)
        (λ (y)
          (cond
            ((eqv? y 'sub1) sub1)
            (else (error 'val-of "unbound variable ~a" y)))))
