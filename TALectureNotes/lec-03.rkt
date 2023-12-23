#lang racket
(require racket/trace)

; (define !
;   (λ (n)
;     (cond
;       ((zero? n) 1)
;       (else (* n (! (sub1 n)))))))

;; (trace !)
 ;;(! 5)

(define loop ;;Will never finish obviously
  (λ (n)
    (cond
      ((zero? n) 0)
      (else (loop n)))))

;; (trace loop)
;; (loop 5)

;; (define even?
;;   (λ (n)
;;     (cond
;;       ((zero? n) #t)
;;       (else (odd? (sub1 n))))))

;; (define odd?
;;   (λ (n)
;;     (cond
;;       ((zero? n) #f)
;;       (else (even? (sub1 n))))))

;; (odd? 19)
;; (even? 46)
;; (even? 13)

;; (+ (f 50) (f 50) (f 50))
;; (letrec ((! (λ (n)
;;            (cond
;;              ((zero? n) 1)
;;              (else (* n (! (sub1 n))))))))
;;   (! 5))

(letrec ((even^? (λ (n) ;;Letrec is a way of defining a temporary recursie function that cannot be accessed outside of the scope. You define a letreec
                  (cond ;;in a very similar manner to a normal function, just without the define 
                    ((zero? n) #t)
                    (else (odd? (sub1 n))))))
         (odd? (λ (n)
                 (cond
                   ((zero? n) #f)
                   (else (even^? (sub1 n)))))))
  (even^? 42))

 ;;(even^? 42)

(define EvenOddClass
  (let ((Even? (λ (this n)
                 (cond
                   ((zero? n) #t)
                   (else ((cadr this) this (sub1 n))))))
        (Odd? (λ (this n)
                (cond
                  ((zero? n) #f)
                  (else ((car this) this (sub1 n)))))))
    (list Even? Odd?)))

;; ((car (cdr EvenOddClass)) EvenOddClass 5)
;; ((car EvenOddClass) EvenOddClass 12)

'(mary had a little lamb whose fleece was white as snow)
'(mary had a little lion whose fleece was white as snow)
(quote (mary had a little lion whose fleece was white as snow))

 ((λ (animal color)
    (quasiquote (mary had a little (unquote animal) whose fleece was (unquote color) as snow)))
  'kangaroo
  'black)
;;Quasiquote essentially acts as a flag to look for something to match. Creates templates that looks for certain things that you specify.
((λ (animal color)
   `(mary had a little ,animal whose fleece was ,color as snow))
 'kangaroo
 'black)


(define parse
  (λ (e)
    (cond
      ((symbol? e) e)
      ((and (pair? e)
            (eqv? (car e) 'λ)
            (pair? (cdr e))
            (pair? (car (cdr e)))
            (symbol? (car (car (cdr e))))
            (null? (cdr (car (cdr e))))
            (pair? (cdr (cdr e)))
            (null? (cdr (cdr (cdr e)))))
       (let ((x (car (car (cdr e))))
             (body (car (cdr (cdr e)))))
         (list 'lambda (list x) (parse body))))
      ((and (pair? e)
            (pair? (cdr e))
            (null? (cdr (cdr e))))
       (let ((rator (car e))
             (rand (car (cdr e))))
         (list (parse rator) (parse rand)))))))

(define parse/match
  (λ (e)
    (match e
      (`,y #:when (symbol? y) y)
      (`(λ (,x) ,body) #:when (symbol? x) `(lambda (,x) ,(parse/match body)))
      (`(,rator ,rand) `(,(parse/match rator) ,(parse/match rand))))))

 (parse/match '(λ (x) (λ (y) (λ (z) (z (x y))))))
 (parse/match 'x)
;;Rator rand are used to go through unmatched cases, to explain any variation, and are often recursively used to go back through the functions with the rator
;;And rand individually
(define e₁=e₂?
  (λ (e₁ e₂)
    (match `(,e₁ ,e₂)
      (`(,y₁ ,y₂)
       #:when (and (symbol? y₁) (symbol? y₂))
       (eqv? y₁ y₂))
      (`((λ (,x₁) ,body₁) (λ (,x₂) ,body₂))
       #:when (and (symbol? x₁) (symbol? x₂))
       (and (eqv? x₁ x₂) (e₁=e₂? body₁ body₂)))
      (`((,rator₁ ,rand₁) (,rator₂ ,rand₂))
       (and (e₁=e₂? rator₁ rator₂) (e₁=e₂? rand₁ rand₂)))
      (else #f))))
(trace e₁=e₂?)
(e₁=e₂? `(λ (x) x) `(λ (y) y))
