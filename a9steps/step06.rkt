#lang racket

;;; We convert this to ANF.
(define fib
  (λ (*n* *k*)
    (cond
      ((<= *n* 1)
       (let* ((*ap-k* *k*)
              (*v* 1))
         (apply-k *ap-k* *v*)))
      (else
       (let* ((*k* (make-sub1-k *n* *k*))
              (*n* (sub1 *n*)))
         (fib *n* *k*))))))

;;; We also convert this to ANF.
(define apply-k
  (λ (*ap-k* *v*)
    (match *ap-k*
      (`(empty-k) *v*)
      (`(make-sub1-k ,n^ ,k^)
       (let* ((*k* (make-sub2-k *v* k^))
              (*n* (sub1 (sub1 n^))))
         (fib *n* *k*)))
      (`(make-sub2-k ,v^ ,k^)
       (let* ((*ap-k* k^)
              (*v* (+ v^ *v*)))
         (apply-k *ap-k* *v*))))))

(define empty-k
  (λ ()
    `(empty-k)))

(define make-sub1-k
  (λ (n^ k^)
    `(make-sub1-k ,n^ ,k^)))

(define make-sub2-k
  (λ (v^ k^)
    `(make-sub2-k ,v^ ,k^)))

(let* ((*k* (empty-k))
       (*n* 5))
  (fib *n* *k*))
