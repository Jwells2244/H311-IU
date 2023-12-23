#lang racket

(provide defrel
         conda condu
         fresh
         disj conj
         run run* run! run*! run-next
         == =/= numbero symbolo absento
         assume all
         succeed fail
         prt)

(struct Relay (query-var $rest)
  #:transparent)

#|microKanren, Ch 10|#

(define nothing
  (vector))

#;
(define (var sym scp alls)
  (vector sym scp nothing alls))

#;
(define var? vector?)

#;
(define (var-val x)
  (vector-ref x 2))

#;
(define (set-var-val! x v)
  (vector-set! x 2 v))

#;
(define (var-scp x)
  (vector-ref x 1))

#;
(define (var-alls x)
  (vector))

(struct var
  (sym scp [val #:mutable] alls)
  #:transparent)

(define (var-has-val? x)
  (not (equal? nothing (var-val x))))

(struct fixed
  (sym)
  #:transparent)

(struct Scope
  ())

(struct State
  (s scp types neqs abs assms alls)
  #:transparent)





(define init-S
  (State (make-immutable-hasheqv)
         (Scope)
         (make-immutable-hasheqv)
         '()
         (make-immutable-hasheqv)
         (make-immutable-hasheqv)
         (set)))

(define (walk v s)
  (cond
    [(var? v)
     (cond
       [(var-has-val? v)
        (walk (var-val v) s)]
       [(hash-has-key? s v)
        (walk (hash-ref s v) s)]
       [else v])]
    [else v]))

(define (ext-s x v s)
  (cond
    [(occurs? x v s) #f]
    [(not (visible? x v s)) #f]
    [else (hash-set s x v)]))

(define (occurs? x v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) (eqv? v x)]
      [(pair? v) (or (occurs? x (car v) s)
                     (occurs? x (cdr v) s))]
      [else #f])))

(define (visible? x v s)
  (let ([v (walk v s)])
    (cond
      [(fixed? v) (set-member? (var-alls x) v)]
      [(pair? v) (and
                  (visible? x (car v) s)
                  (visible? x (cdr v) s))]
      [else #t])))























(define (unify u v s scp new-pairs)
  (let ([u (walk u s)]
        [v (walk v s)])
    (cond
      [(eqv? u v) (cons s new-pairs)]
      [(var? u)
       (cond
         [(eqv? (var-scp u) scp)
          (and (not (occurs? u v s))
               (visible? u v s)
               (begin
                 (set-var-val! u v)
                 (cons s `((,u . ,v) . ,new-pairs))))]
         [else
          (go-on ([s (ext-s u v s)])
                 (cons s `((,u . ,v) . ,new-pairs)))])]
      [(var? v) (unify v u s scp new-pairs)]
      [(and (pair? u) (pair? v))
       (go-on ([`(,s . ,new-pairs)
                (unify (car u) (car v) s scp new-pairs)]
               [`(,s . ,new-pairs)
                (unify (cdr u) (cdr v) s scp new-pairs)])
              (cons s new-pairs))]
      [else #f])))

(define (== u v)
  (λ (S)
    (go-on ([`(,s . ,new-pairs)
             (unify u v (State-s S) (State-scp S) '())]
            [neqs (validate-neqs (State-neqs S) s)]
            [types (validate-types new-pairs (State-types S))]
            [`(,neqs . ,abs)
             (validate-abs new-pairs neqs (State-abs S) s)])
      `(,(struct-copy
           State S [s s] [neqs neqs] [types types] [abs abs]))
      '())))

(define (succeed S) `(,S))

(define (fail S) '())

(define ($append $1 $2)
  (cond
    [(null? $1) $2]
    [(pair? $1) (cons (car $1) ($append (cdr $1) $2))]
    [else (λ () ($append $2 ($1)))]))

(define ($take n $)
  (cond
    [(and n (zero? n)) `(() . ,$)]
    [(null? $) `(() . ,$)]
    [(pair? $)
     (match-let ([`(,d . ,$^) ($take (and n (sub1 n))
                                     (cdr $))])
       `(,(cons (car $) d) . ,$^))]
    [else ($take n ($))]))


(define ($append-map g $)
  (cond
    [(null? $) '()]
    [(pair? $)
     ($append (g (car $)) ($append-map g (cdr $)))]
    [else (λ () ($append-map g ($)))]))

(define call/fresh
  (λ (name f)
    (λ (S)
      ((f (var name (State-scp S) nothing (State-alls S)))
       S))))

(define (reify-name n)
  (string->symbol (string-append "_" (number->string n))))

(define (walk* v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) v]
      [(pair? v) (cons (walk* (car v) s)
                       (walk* (cdr v) s))]
      [else v])))

(define (reify-s v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) (let ([n (hash-count s)])
                  (let ([rn (reify-name n)])
                    (hash-set s v rn)))]
      [(pair? v) (let ([s (reify-s (car v) s)])
                   (reify-s (cdr v) s))]
      [else s])))

(define (reify v)
  (λ (S)
    (let ([v (walk* v (State-s S))])
      (let ([names (reify-s v (make-immutable-hasheqv))])
        (walk* v names)))))

(define (run-goal n g)
  ($take n (g init-S)))

(define ((ifte g1 g2 g3) S)
  (let loop ([$ (g1 S)])
    (cond
      [(null? $) (g3 S)]
      [(pair? $)
       ($append-map g2 $)]
      [else (λ () (loop ($)))])))

(define ((once g) S)
  (let loop ([$ (g S)])
    (cond
      [(null? $) '()]
      [(pair? $) (cons (car $) '())]
      [else (λ () (loop ($)))])))








#|macros, connecting wires|#
(define (D s∞ gs s)
  (cond
    [(null? gs) s∞]
    [else
     ($append s∞
              (D ((car gs) s) (cdr gs) s))]))

(define (disj . gs)
  (λ (s)
    ;; this new scope is necessary here
    (let ([s (struct-copy State s [scp (Scope)])])
      (cond
        [(null? gs) (fail s)]
        [else (D ((car gs) s) (cdr gs) s)]))))

(define (C gs s∞)
  (cond
    [(null? gs) s∞]
    [else
     (C (cdr gs)
        ($append-map (car gs) s∞))]))

(define (conj . gs)
  (λ (s)
    (cond
      [(null? gs) (succeed s)]
      [else
       (C (cdr gs) ((car gs) s))])))

(define (make-goals formals argss)
  (cond
    [(null? argss) fail]
    [else (disj (== formals (car argss))
                (make-goals formals (cdr argss)))]))

(define-syntax defrel
  (syntax-rules ()
    [(defrel (name x ...) g ...)
     (define name
       (λ (x ...)
         (λ (S)
           (λ ()
             (let ([sup (State-assms S)]
                   [def (conj g ...)])
               ((if (dict-has-key? sup 'name)
                    (disj
                     (make-goals
                      `(,x ...)
                      (dict-ref sup 'name))
                     def)
                    def)
                S))))))]))

#;
(define-syntax defrel
  (syntax-rules ()
    [(defrel (name x ...) g ...)
     (define name
       (λ (x ...)
         (λ (S)
           (λ ()
             (let ([sup (State-assms S)]
                   [def (conj g ...)])
               ((if (dict-has-key? sup 'name)
                    (disj2
                      (make-goals
                        `(,x ...)
                        (dict-ref sup 'name))
                      def)
                    def)
                S))))))]))

(define (reify-type-predicate p)
  (cond
    [(eqv? p symbol?) 'symbolo]
    [(eqv? p number?) 'numbero]
    [else (error "oops")]))






















(define (reify/constraints v)
  (λ (S)
    (let*
        ([v (walk* v (State-s S))]
         [names (reify-s `(,v ,(State-neqs S)
                              ,(dict-values (State-abs S)))
                         (make-immutable-hasheqv))]
         [reified-term (walk* v names)]
         [reified-types
          (foldr
           (λ (var kinds)
             (if (dict-has-key? (State-types S) var)
                 `((,(reify-type-predicate
                      (dict-ref (State-types S) var))
                    ,(walk* var names)) . ,kinds)
                 kinds))
           '()
           (dict-keys names))]
         [reified-neqs
          (remove-duplicates
           (foldr
            (λ (list-of-pairs neqs)
              (append
               (map
                (λ (pr)
                  `(=/= ,(walk* (car pr) names)
                        ,(walk* (cdr pr) names)))
                list-of-pairs)
               neqs))
           '()
           (State-neqs S)))]
         [reified-abs
          (foldr
           (λ (var abs)
             (if (dict-has-key? names var)
                 `((absento
                    ,(walk* var names)
                    ,(walk*
                      (dict-ref (State-abs S) var)
                      names))
                   . ,abs)
                 abs))
           '()
           (dict-keys (State-abs S)))])
      `(,reified-term
        ,reified-types
        ,reified-neqs
        ,reified-abs))))

(define-syntax run
  (syntax-rules ()
    [(run n (x₀ x ...) g ...)
     (run n q (fresh (x₀ x ...)
                     (== `(,x₀ ,x ...) q)
                     g ...))]
    [(run n q g ...)
     (let ([q (var 'q (Scope) nothing (set))])
       (match-let ([r/c (reify/constraints q)]
                   [`(,r-g . ,$rest) (run-goal n (conj g ...))])
         `(,(map r/c r-g) . ,(Relay q $rest))))]))

(define-syntax run*
  (syntax-rules ()
    [(run* q g ...) (run #f q g ...)]))

(define-syntax run*!
  (syntax-rules ()
    [(run*! q g ...) (run! #f q g ...)]))

(define-syntax run!
  (syntax-rules ()
    [(run! n q g ...)
     (match-let ([`(,res . ,next-relay) (run n q g ...)])
       (format-res res (if n n -1))
       (let ([next-relay^ next-relay])
         (λ (n^)
           (match-let ([`(,res . ,next-relay^^)
                        (run-next n^ next-relay^)])
             (set! next-relay^ next-relay^^)
             (format-res res n^)))))]))

(define run-next
  (λ (n relay)
    (let ([q (Relay-query-var relay)]
          [$rest (Relay-$rest relay)])
      (match-let ([`(,solutions . ,$rest^) ($take n $rest)])
        `(,(map (reify/constraints q) solutions)
          . ,(Relay q $rest^))))))

(define-syntax fresh
  (syntax-rules ()
    ;; now both lines are required, but do you really know why?
    ((fresh () g) g)
    ((fresh () g gs ...) (conj g gs ...))
    ((fresh (x0 x ...) g ...)
     (call/fresh 'x_0
       (lambda (x0)
         (fresh (x ...) g ...))))))

(define-syntax conda
  (syntax-rules ()
    [(conda (g0 g ...)) (conj g0 g ...)]
    [(conda (g0 g ...) ln ...)
     (ifte g0 (conj g ...) (conda ln ...))]))

(define-syntax condu
  (syntax-rules ()
    [(condu (g0 g ...) ...)
     (conda ((once g0) g ...) ...)]))

#|other constraints|#

(define ((prt c) S)
  (let ([s (State-s S)])
    (begin (displayln (walk* c s))
           `(,S))))

(define (validate-neqs neqs s)
  (cond
    [(null? neqs) '()]
    [else
     (go-on ([new-car (unify-all (car neqs) s '())])
       (if (null? new-car)
           #f
           (go-on ([new-cdr (validate-neqs (cdr neqs) s)])
             (cons new-car new-cdr)))
       (validate-neqs (cdr neqs) s))]))



(define (unify-all ls s new-pairs)
  (cond
    [(null? ls) new-pairs]
    [else (go-on ([`(,s . ,new-pairs)
                   (unify (car (car ls)) (cdr (car ls))
                          s (Scope) new-pairs)])
            (unify-all (cdr ls) s new-pairs))]))

(define (validate-types ls types)
  (cond
    [(null? ls) types]
    [else (go-on ([types (propogate-type (car ls) types)]
                  [types (validate-types (cdr ls) types)])
                 types)]))

(define (propogate-type pr types)
  (let ([u (car pr)]
        [v (cdr pr)])
    (cond
      [(var? v)
       (let ([u-type (hash-ref types u #f)]
             [v-type (hash-ref types v #f)])
         (cond
           [(and u-type v-type) (and (eqv? u-type v-type) types)]
           [u-type (hash-set types v u-type)]
           [v-type (hash-set types u v-type)]
           [else types]))]
      [else
       (let ([u-type (hash-ref types u #f)])
         (cond
           [u-type (and (u-type v) types)]
           [else types]))])))

(define (unicons x ls)
  (if (memv x ls) ls (cons x ls)))

(define (not-appears u v neqs abs s)
  (let ([u (walk u s)]
        [v (walk v s)])
    (cond
      [(var? v) (let ([v-abs (hash-ref abs v #f)])
                  (cons (cons `((,v . ,u)) neqs)
                        (hash-set abs v (unicons u (or v-abs '())))))]
      [(pair? v)
       (go-on ([`(,neqs . ,abs)
                (not-appears u (car v) neqs abs s)])
         (not-appears u (cdr v) neqs abs s))]
      [else (and (not (eqv? u v)) (cons neqs abs))])))

(define (validate-abs ls neqs abs s)
  (cond
    [(null? ls) (cons neqs abs)]
    [else
     (let ([pr (car ls)])
       (let ([u (car pr)]
             [v (cdr pr)])
         (let ([u-abs (hash-ref abs u #f)])
           (if u-abs
               (go-on ([`(,neqs . ,abs)
                        (propogate-abs u-abs v neqs abs s)])
                 (validate-abs (cdr ls) neqs abs s))
               (validate-abs (cdr ls) neqs abs s)))))]))


(define (propogate-abs ls t neqs abs s)
  (cond
    [(null? ls) (cons neqs abs)]
    [else (go-on ([`(,neqs . ,abs)
                   (not-appears (car ls) t neqs abs s)])
            (propogate-abs (cdr ls) t neqs abs s))]))

(define (=/= u v)
  (λ (S)
    (go-on ([`(,_ . ,new-pairs)
             (unify u v (State-s S) (Scope) '())])
           (if (null? new-pairs)
               '()
               `(,(struct-copy
                    State S
                    [neqs (cons new-pairs
                                (State-neqs S))])))
           `(,S))))

(define (numbero u)
  (typeo number? u))

(define (symbolo u)
  (typeo symbol? u))

(define (typeo pred u)
  (λ (S)
    (let ([u (walk u (State-s S))])
      (cond
        [(var? u)
         (let ([u-type (hash-ref (State-types S) u #f)])
           (cond
             [u-type (if (eqv? u-type pred) `(,S) '())]
             [else
              `(,(struct-copy
                  State S
                  [types (hash-set
                          (State-types S) u pred)]))]))]
        [(pred u) `(,S)]
        [else '()]))))

(define (absento u v)
  (λ (S)
    (go-on ([`(,neqs . ,abs)
             (not-appears
              u v
              (State-neqs S) (State-abs S) (State-s S))])
      `(,(struct-copy
           State S [neqs neqs] [abs abs]))
      '())))

(define-syntax assume
  (syntax-rules ()
    [(assume (name arg ...) g ...)
     (λ (S)
       (let* ([sup (State-assms S)]
              [assumptions (dict-ref sup 'name '())]
              [assumptions (cons `(,arg ...) assumptions)]
              [sup (dict-set sup 'name assumptions)])
         ((conj g ...)
          (struct-copy State S [assms sup]))))]))



(define call/all
  (λ (name f)
    (λ (S)
      (let ([x (fixed name)])
        ((f x)
         (struct-copy
           State S [alls (set-add (State-alls S) x)]))))))

(define-syntax all
  (syntax-rules ()
    [(fresh () g ...) (conj g ...)]
    [(fresh (x0 x ...) g ...)
     (call/all 'x0
               (λ (x0)
                 (all (x ...) g ...)))]))

#|syntax sugars|#

(define-syntax go-on
  (syntax-rules ()
    [(_ () then) then]
    [(_ () then alter) then]
    [(_ ([p0 e0] [p e] ...) then)
     (cond
       [e0 => (λ (v) (match v
                       [p0 (go-on ([p e] ...) then)]))]
       [else #f])]
    [(_ ([p0 e0] [p e] ...) then alter)
     (cond
       [e0 => (λ (v)
                (match v
                  [p0 (go-on ([p e] ...) then alter)]))]
       [else alter])]))

(define (remove-duplicates l)
  (set->list (list->set l)))





























(define format-res
  (λ (res query-n)
    (newline)
    (printf "Results requested:\n")
    (let ([res-len (length res)])
      (for ([i res-len]
            [r res])
        (printf "==========================\n")
        (printf "Result ")
        (when (not (= res-len 1))
          (printf "#~a" (add1 i)))
        (printf ": ~a \n" (car r))
        (when (not (null? (cadr r)))
          (printf "Typeos ")
          (when (not (= res-len 1))
            (printf "#~a" (add1 i)))
          (printf ": ~a \n" (cadr r)))
        (when (not (null? (caddr r)))
          (printf "Not equals ")
          (when (not (= res-len 1))
            (printf "#~a" (add1 i)))
          (printf ": ~a \n" (caddr r)))
        (when (not (null? (cadddr r)))
          (printf "Absentos ")
          (when (not (= res-len 1))
            (printf "#~a" (add1 i)))
          (printf ": ~a \n" (cadddr r))))
      (when (or (<= query-n 0) (< res-len query-n))
        (printf "==========================\n")
        (printf "Only ~a results" res-len)
        (when (> query-n 0)
            (printf " out of ~a requested\n" query-n)))
      (newline))))
