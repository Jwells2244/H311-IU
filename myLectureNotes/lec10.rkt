#lang racket
(require racket/trace)

(define sub2 (λ (n) (sub1 (sub1 n))))

(define fib
  (λ (n)
    (cond
      ((zero? n) 1)
      ((zero? (sub1 n)) 1)
      (else (+ (fib (sub1 n))
               (fib (sub2 n)))))))
(fib 5)

(define fib-cps
  (λ (n k)
    (cond
      ((zero? n) (k 1))
      ((zero? (sub1 n)) (k 1))
      (else (fib-cps (sub1 n) (λ (v) (fib-cps (sub2 n) (λ (w)
                                                         (k (+ v w))))))))))

(trace fib-cps)
(fib-cps 5 (lambda (x) x))

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
(trace fib-memo)
(fib-memo 5)