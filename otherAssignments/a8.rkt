#lang racket
(require racket/trace)

; 1.

#|
cpsed, ri'd, in anormal form. This differs from the conventional implementation which is just naturally recursing twice with two different calls,
one where n is -1 and the other where n is -2, and adding the results of those calls together. Instead we are passing a continuation along with the n
because it is in continuation passing style, and that continuation allows us to evaluate items off the stack prodedurally, rather than recursing into
the stack multiple times, it is all done at the level where the continuation is applied. It is also representation independent which means it
uses higher order functions to accomplish things that wouldve been calls within the body of the function. This ensures that we can change things easier,
instead of modifying the body each time we are renaming a variable or registerizing it, and so on, basically just abstracting parts of the function, which
hides the representation of the funciton.
The anormal form is when we originally let* the variables before we pass them into the next call, which is different from the normal implementation which
would just have the variables be passed normally and honestly I don't know the benefit.
|#

(define *v* #f)
(define *k* #f)
(define *a-k* #f)
(define *n* #f)
  
(define apply-k
  (λ ()
    (match *a-k*
      (`(make-inner-fib-k ,fib-sub1-n^ ,k^)
       (begin (set! *a-k* k^)
              (set! *v* (+ fib-sub1-n^ *v*))
              (apply-k)))
      (`(make-outer-fib-k ,n^ ,k^)
       (begin (set! *k* (make-inner-fib-k *v* k^))
              (set! *n* (sub1 (sub1 n^)))
              (fib-cps)))
      (`(init-k) *v*))))
 
(define make-inner-fib-k
  (λ (fib-sub1-n^ k^)
    `(make-inner-fib-k ,fib-sub1-n^ ,k^)))
 
(define make-outer-fib-k
  (λ (n^ k^)
    `(make-outer-fib-k ,n^ ,k^)))
 
(define init-k
  (λ ()
    `(init-k)))
 
(define fib-cps
  (λ ()
    (cond
      ((zero? *n*)
       (begin (set! *a-k* *k*)
              (set! *v* 1)
              (apply-k)))
      ((zero? (sub1 *n*))
       (begin (set! *a-k* *k*)
              (set! *v* 1))
       (apply-k))
    (else
     (begin (set! *k* (make-outer-fib-k *n* *k*))
            (set! *n* (sub1 *n*))
            (fib-cps))))))

(begin
  (set! *k* (init-k))
  (set! *n* 5)
  (fib-cps))

; 2.

(define times-cps
  (λ (ls k)
    (cond
      [(null? ls) (lambda () (apply-k-times k 1))]
      [(zero? (car ls)) (lambda () (apply-k-times k 0))]
      [else (lambda () (times-cps (cdr ls) (make-times-cdr-k ls k)))])))

(define apply-k-times
  (lambda (k v)
    (match k
      [`(empty-k ,jumpout) (lambda () (begin (printf
                                              "once"
                                              (jumpout v))))]
      [`(make-times-cdr-k ,ls ,k)
       (lambda () (apply-k-times k (* (car ls) v)))])))

(define make-times-cdr-k
  (lambda (ls k)
    `(make-times-cdr-k ,ls ,k)))

(define empty-k
  (λ (jumpout)
    `(empty-k ,jumpout)))

(define tramp
  (λ (th)
    (tramp (th))))

(trace times-cps)
(trace tramp)

(let/cc jumpout
  (tramp (times-cps '(9 8 7 6 5 4 3 2 1) (empty-k jumpout))))

(define bi-tramp
  (λ (th1 th2)
    (bi-tramp th2 (th1))))

(let/cc jumpout
  (bi-tramp
   (times-cps '(2 2 2 2 2 2 2 2 2 2 2 2 2 2 2) (empty-k jumpout))
   (times-cps '(9 8 7 6 5 4 3 2 1) (empty-k jumpout))))