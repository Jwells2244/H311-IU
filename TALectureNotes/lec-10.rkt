#lang racket
(require racket/trace)

;;Continuation passing style
(define length
  (λ (ls)
    (cond
      ((null? ls) 0)
      (else (add1 (length (cdr ls)))))))

;We add a k into the arguments of this function. Anytime something simple is being returned, (and a lambda is considered simple)
;we apply k to it, applying the current continuation to it
(define length-cps
  (λ (ls k)
    (cond
      ((null? ls) (k 0))
      (else (length-cps (cdr ls)
                        (λ (v)
                          (k (add1 v))))))))
;When we have an extension of the environment, this means it is not simple and we will define another lambda expression,
;usually v, and then handle the arguments of the function inside that v lambda, usually doing something with v and then applying k to that

;;Writing programs in continuation passing style allows us to emphasize the control flow of our program and keep things simple
;A normal function will just expand on its arguments as it is called recursively, but a cpsd function will only be a snake, meaning
;that each line of tracing the function will just be one action, the cps makes it so it does not remember that it will be applying something again,
;it just evaluates what it is given at that current continuation.

;; (trace length)
;; (length '(1 2 3 4 5))
;; (trace length-cps)
;; (length-cps '(1 2 3 4 5) (λ (x) x))
;Standard sub2
(define sub2 (λ (n) (sub1 (sub1 n))))

;standard fib
(define fib
  (λ (n)
    (cond
      ((zero? n) 1)
      ((zero? (sub1 n)) 1)
      (else (+ (fib (sub1 n))
               (fib (sub2 n)))))))
(trace fib)
(fib 5)

;Cps'd version of fib, where there are two non simple return values, so we have to define a second extension lambda, in this case w,
;in order to handle this. But both of the 1 cases just return 1, which is simple, so we can apply k to it.
(define fib-cps
  (λ (n k)
    (cond
      ((zero? n) (k 1))
      ((zero? (sub1 n)) (k 1))
      (else (fib-cps (sub1 n)
                     (λ (v)
                       (fib-cps (sub2 n)
                                (λ (w)
                                  (k (+ v w))))))))))
;; (trace fib-cps)
;; (fib-cps 5 (lambda (x) x))
;; (time (fib-cps 40 (lambda (x) x)))
;This is a concept called memoization, which allows us to store previous values if we know we are going to do a lot of repeat calculations,
;such as in fibonacci. This allows us to speed up the running of our program exponentially.
(define fib-memo
  (let ((memo '((0 . 1) (1 . 1))))
    (λ (n)
      (cond
        ((assv n memo) => (λ (p)
                            (cdr p)))
        (else (let ((rst (+ (fib-memo (sub1 n))
                            (fib-memo (sub2 n)))))
                (set! memo (cons `(,n . ,rst) memo))
                rst))))))
;; (trace fib-memo)
;; (fib-memo 6)
;(time (fib-memo 40000))

;This is rember*8, which basically takes out any 8s from the input
;writing this normally, reverse engineering it.
;(define rember*8
;  (λ (ls)
;    (cond
;      ((null? ls) '())
;      ((pair? (car ls))
;       (cons (rember*8 (car ls)) (rember*8 (cdr ls))))
;      ((eqv? 8 (car ls)) (rember*8 (cdr ls)))
;      (else
;       ((cons (car ls) (rember*8 (cdr ls))))))))
(define rember*8-cps
  (λ (ls k)
    (cond
      ((null? ls) (k '()))
      ((pair? (car ls))
       (rember*8-cps (car ls)
                      (lambda (v)
                        (rember*8-cps (cdr ls)
                                      (lambda (w) (k (cons v w)))))))
      ((eqv? 8 (car ls)) (rember*8-cps (cdr ls) k))
      (else
       (rember*8-cps (cdr ls)
                      (lambda (v)
                        (k (cons (car ls) v))))))))

;(define rember*8-cps
;  (λ (ls k)
;    (cond
;      ((null? ls) (k '()))
;      ((pair? (car ls))
;       (rember*8-cps (car ls)
;                     (λ (v)
;                       (rember*8-cps (cdr ls)
;                                     (λ (w)
;                                       (k (cons v w)))))))
;      ((eqv? 8 (car ls)) (rember*8-cps (cdr ls) k))
;      (else
;       (rember*8-cps (cdr ls)
;                     (λ (v)
;                       (k (cons (car ls) v))))))))

(trace rember*8)
;(rember*8 '(8 ((1 8 3 8 2 8 2) (3 27 8))))
(trace rember*8-cps)
(rember*8-cps '(8 ((1 8 3 8 2 8 2) (3 27 8)))
              (λ (x) x))
