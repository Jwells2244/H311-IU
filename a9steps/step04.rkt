#lang racket

(define fib
  (λ (n k)
    (cond
      ((<= n 1) (apply-k k 1))
      (else
       (fib (sub1 n) (make-sub1-k n k))))))

;;; We use data structural representation of continuations.
(define apply-k
  (λ (k v)
    (match k
      (`(empty-k) v)
      (`(make-sub1-k ,n^ ,k^)
       (fib (sub1 (sub1 n^))
            (make-sub2-k v k^)))
      (`(make-sub2-k ,v^ ,k^)
       (apply-k k^ (+ v^ v))))))

(define empty-k
  (λ ()
    `(empty-k)))

(define make-sub1-k
  (λ (n^ k^)
    `(make-sub1-k ,n^ ,k^)))

(define make-sub2-k
  (λ (v^ k^)
    `(make-sub2-k ,v^ ,k^)))

(fib 5 (empty-k))
