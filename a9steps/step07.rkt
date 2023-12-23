#lang racket

;;; We registerize fib. We first replace function arguments
;;; with global registers, and then replace let* bindings
;;; with set!.
(define *n* #f)
(define *k* #f)
(define *ap-k* #f)
(define *v* #f)

(define fib
  (λ ()
    (cond
      ((<= *n* 1)
       (begin
         (set! *ap-k* *k*)
         (set! *v* 1)
         (apply-k)))
      (else
       (begin
         (set! *k* (make-sub1-k *n* *k*))
         (set! *n* (sub1 *n*))
         (fib))))))

(define apply-k
  (λ ()
    (match *ap-k*
      (`(empty-k) *v*)
      (`(make-sub1-k ,n^ ,k^)
       (begin
         (set! *k* (make-sub2-k *v* k^))
         (set! *n* (sub1 (sub1 n^)))
         (fib)))
      (`(make-sub2-k ,v^ ,k^)
       (begin
         (set! *ap-k* k^)
         (set! *v* (+ v^ *v*))
         (apply-k))))))

(define empty-k
  (λ ()
    `(empty-k)))

(define make-sub1-k
  (λ (n^ k^)
    `(make-sub1-k ,n^ ,k^)))

(define make-sub2-k
  (λ (v^ k^)
    `(make-sub2-k ,v^ ,k^)))

(begin
  (set! *k* (empty-k))
  (set! *n* 5)
  (fib))
