#lang racket

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

;;; We rename these helper function arguments with a ^, to
;;; avoid potential name conflicts with the arguments of
;;; apply-k and fib.
(define make-sub1-k
  (λ (n^ k^)
    (λ (v)
      (fib (sub1 (sub1 n^))
           (make-sub2-k v k^)))))

(define make-sub2-k
  (λ (v^ k^)
    (λ (w)
      (apply-k k^ (+ v^ w)))))

(fib 5 (empty-k))
