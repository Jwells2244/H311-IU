#lang racket
(require racket/trace)
;;Registerization, Jonathan notes
;;This is our cpsd interpreter, where whenever a value is being returned, the continuation k is applied to that value
(define value-of-cps
  (λ (e env k)
    (match e
      (`,n #:when (number? n) (k n)) ;Like here
      (`,y #:when (symbol? y) (env y k)) ;And here
      (`(+ ,exp1 ,exp2)
       (value-of-cps exp1 env
                     (λ (v)
                       (value-of-cps exp2 env
                                     (λ (w)
                                       (k (+ v w))))))) ;And here
      (`(catch ,c-name ,c-exp)
       #:when (symbol? c-name)
       (value-of-cps c-exp
                     (λ (y k^)
                       (cond
                         ((eqv? y c-name) (k^ k)) ;Here, we get a little fancy with it tho, because we are dealing with catch and pitch
                         (else (env y k^))))
                     k))
      (`(pitch ,exp1 ,c-exp)
       ;; (value-of-cps c-exp env
       ;;               (λ (k^)
       ;;                 (value-of-cps exp1 env k^)))
       (value-of-cps exp1 env
                     (λ (v)
                       (value-of-cps c-exp env
                                     (λ (k^) ;Here, because once again dealing with catch and pitch, means that we have to extend our environment with the new continuation
                                       (k^ v)))))) ;that is given from pitch to catch
       (`(λ (,var) ,body)
        #:when (symbol? var)
        (k (λ (arg k^^)
             (value-of-cps body
                           (λ (y k^)
                             (cond
                               ((eqv? y var) (k^ arg))
                               (else (env y k^))))
                           k^^))))
       (`(,rator ,rand)
        (value-of-cps rator
                      env
                      (λ (clos)
                        (value-of-cps rand
                                      env
                                      (λ (v)
                                        (clos v k)))))))))

;; (trace value-of-cps)

(value-of-cps
 '(λ (a) 5)
 (λ (y k) (error "Not found!"))
 (λ (v) v))

(value-of-cps
 '((λ (a) a) 5)
 (λ (y k) (error "Not found!"))
 (λ (v) v))

(value-of-cps
 '(+ (+ 3 8) 5)
 (λ (y k) (error "Not found!"))
 (λ (v) v))

(value-of-cps
 '(catch k (+ 3 5))
 (λ (y k) (error "Not found!"))
 (λ (v) v))

(value-of-cps
 '(catch k (+ 3 (pitch (+ 2 6) k)))
 (λ (y k) (error "Not found!"))
 (λ (v) v))

; (value-of-cps
;  '(+ 4 (catch k (+ 3 (pitch (+ 2 6) k))))
;  (λ (y k) (error "Not found!"))
;  (λ (v) v))

;(let/cc k (+ 3 (k (+ 2 6))))
;(let/cc k (+ 4 (+ 3 (k (+ 2 6)))))
;Standard cbv interpeter which evaluates the arguments and a reference to the result is passed into a function.
;except this can also hanle let/cc, which deals with continuations.
(define val-of
  (λ (e env)
    (match e
      (`,n #:when (number? n) n)
      (`,s #:when (symbol? s) (env s))
      (`(+ ,exp1 ,exp2)
       (+ (val-of exp1 env) (val-of exp2 env)))
      (`(catch ,c-name ,c-exp)
       #:when (symbol? c-name)
       (let/cc k (val-of c-exp
                         (λ (y)
                           (cond
                             ((eqv? y c-name) k)
                             (else (env y)))))))
      (`(pitch ,exp1 ,c-exp)
       ((val-of c-exp env) (val-of exp1 env)))
      (`(λ (,var) ,body)
       #:when (symbol? var)
       (λ (arg)
         (val-of body
                 (λ (y)
                   (cond
                     ((eqv? y var) arg)
                     (else (env y)))))))
      (`(,rator ,rand)
       ((val-of rator env) (val-of rand env))))))
(printf "Reached here ~n")
;Evaluates to 8 because it is a simple math expression
(val-of
 '(catch k (+ 3 5))
 (λ (y) (error "Not found!")))
;Evals to 8 again because the +2 6 is pitched to the catch as k, which skips the +3
(val-of
 '(catch k (+ 3 (pitch (+ 2 6) k)))
 (λ (y) (error "Not found!")))
;This evaluates to 12, because the 8 is pitched to the catch as k, skipping the +3, but then gets evaluated with the +4
(val-of
 '(+ 4 (catch k (+ 3 (pitch (+ 2 6) k))))
 (λ (y) (error "Not found!")))
;This evaluates to 4, because there are two catch/pitch combinations, and the inner one would evaluate to 8, if there was not a second pitch that pitched
;back to the first catch, pitching the value of k as 4 back
(val-of
 '(catch k (+ (pitch 4 k) (catch k (+ 3 (pitch (+ 2 6) k)))))
 (λ (y) (error "Not found!")))
;Just another cps demo.
(define insertL*
  (λ (q r ls)
    (cond
      ((null? ls) null)
      ((pair? (car ls))
       (cons (insertL* q r (car ls))
             (insertL* q r (cdr ls))))
      ((eqv? (car ls) q)
       (cons r (cons q (insertL* q r (cdr ls)))))
      (else
       (cons (car ls)
             (insertL* q r (cdr ls)))))))

(insertL* 'x 'y '(a x (b x y z (x u)) j l z))

(define insertL*-cps
  (λ (q r ls k)
    (cond
      ((null? ls) (k null))
      ((pair? (car ls))
       (insertL*-cps q r (car ls)
                     (λ (v)
                       (insertL*-cps q r (cdr ls)
                                     (λ (w)
                                       (k (cons v w)))))))
      ((eqv? (car ls) q)
       (insertL*-cps q r (cdr ls)
                     (λ (v)
                       (k (cons r (cons q v))))))
      (else
       (insertL*-cps q r (cdr ls)
                     (λ (v)
                       (k (cons (car ls) v))))))))

;; (trace insertL*-cps)
(insertL*-cps 'x 'y '(a x (b x y z (x u)) j l z)
              (λ (v) v))

;;Representation independent with respect to continuations
;Basically scooped out the apply-k parts and turned the entire thing into tagged list data structure representation with higher order functions as well.
(define insertL*-cps-ri
  (λ (q r ls k)
    (cond
      ((null? ls) (apply-k k null))
      ((pair? (car ls))
       (insertL*-cps-ri q r (car ls) (make-pair-car-k q r ls k)))
      ((eqv? (car ls) q)
       (insertL*-cps-ri q r (cdr ls) (make-car-ls-k r q k)))
      (else
       (insertL*-cps-ri q r (cdr ls) (make-cdr-ls-k ls k))))))

(define apply-k
  (λ (k v)
    (match k
      (`(empty-k) v)
      (`(make-cdr-ls-k ,ls ,k) (apply-k k (cons (car ls) v)))
      (`(make-car-ls-k ,r ,q ,k) (apply-k k (cons r (cons q v))))
      (`(make-pair-car-k ,q ,r ,ls ,k)
       (insertL*-cps-ri q r (cdr ls) (make-pair-cdr-k v k)))
      (`(make-pair-cdr-k ,v^ ,k) (apply-k k (cons v^ v))))))

(define make-cdr-ls-k
  (λ (ls k)
    `(make-cdr-ls-k ,ls ,k)))

(define make-car-ls-k
  (λ (r q k)
    `(make-car-ls-k ,r ,q ,k)))

(define make-pair-car-k
  (λ (q r ls k)
    `(make-pair-car-k ,q ,r ,ls ,k)))

(define make-pair-cdr-k
  (λ (v k)
    `(make-pair-cdr-k ,v ,k)))

(define empty-k
  (λ ()
    `(empty-k)))

;(trace insertL*-cps-ri)
(insertL*-cps-ri 'x 'y '(a x (b x y z (x u)) j l z)
                 (empty-k))
