#lang racket
(require "parenthec.rkt")

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
         (set! *k* (cont_make-sub1-k *n* *k*))
         (set! *n* (sub1 *n*))
         (fib))))))

;;; We replace match with a union-case. union-case takes a
;;; value and a union name, and matches the value with each
;;; case of the union. Here don't need to use quasiquote,
;;; comma, and the "<union>_" in the union pattern.
(define apply-k
  (λ ()
    (union-case *ap-k* cont
      ((empty-k) *v*)
      ((make-sub1-k n^ k^)
       (begin
         (set! *k* (cont_make-sub2-k *v* k^))
         (set! *n* (sub1 (sub1 n^)))
         (fib)))
      ((make-sub2-k v^ k^)
       (begin
         (set! *ap-k* k^)
         (set! *v* (+ v^ *v*))
         (apply-k))))))

;;; We replace helper functions with define-union.  When we
;;; invoke those helper functions, we add a "<union>_"
;;; before each function name, where <union> is the union
;;; name, for example, cont here.
(define-union cont
  (make-sub1-k n^ k^)
  (make-sub2-k v^ k^)
  (empty-k))

#|
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
|#

(begin
  (set! *k* (cont_empty-k))
  (set! *n* 5)
  (fib))
