#lang racket
(require racket/trace)
;;; Next we do RI with respect to continuations, and we use
;;;; functional representation of continuations.
;(define fib
;  (λ (n k)
;    (cond
;      ((<= n 1) (k 1))
;      (else
;       (fib (sub1 n)
;            (λ (v)
;              (fib (sub1 (sub1 n))
;                   (λ (w)
;                     (k (+ v w))))))))))
(define fib
  (λ (n k)
    (cond
      ((<= n 1) (apply-k k 1))
      (else
       (fib (sub1 n) (make-sub1-k n k))))))

(define apply-k
  (λ (k v)
    (k v)))

(define empty-k
  (λ ()
    (λ (v) v)))

(define make-sub1-k
  (λ (n k)
    (λ (v)
      (fib (sub1 (sub1 n))
           (make-sub2-k v k)))))

(define make-sub2-k
  (λ (v k)
    (λ (w)
      (apply-k k (+ v w)))))
(trace fib)
(fib 5 (empty-k))
