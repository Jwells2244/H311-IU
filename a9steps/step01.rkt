#lang racket

;;; The we CPS it.
(define fib
  (λ (n k)
    (cond
      ((<= n 1) (k 1))
      (else
       (fib (sub1 n)
            (λ (v)
              (fib (sub1 (sub1 n))
                   (λ (w)
                     (k (+ v w))))))))))

(fib 5 (λ (v) v))
