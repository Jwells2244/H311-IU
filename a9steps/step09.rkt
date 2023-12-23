#lang racket
(require "parenthec.rkt")

;;; We replace our global definitions with define-registers.
(define-registers *n* *k* *ap-k* *v*)
;;; We add a program counter.
(define-program-counter *pc*)

;;; We replace function definitions with define-label.
(define-label fib
  (cond
    ((<= *n* 1)
     (begin
       (set! *ap-k* *k*)
       (set! *v* 1)
       ;; We also replace function invocation with set
       ;; *pc*.
       (set! *pc* apply-k)))
    (else
     (begin
       (set! *k* (cont_make-sub1-k *n* *k*))
       (set! *n* (sub1 *n*))
       (set! *pc* fib)))))

(define-label apply-k
  (union-case *ap-k* cont
    ((empty-k jumpout) (jumpout *v*))
    ((make-sub1-k n^ k^)
     (begin
       (set! *k* (cont_make-sub2-k *v* k^))
       (set! *n* (sub1 (sub1 n^)))
       (set! *pc* fib)))
    ((make-sub2-k v^ k^)
     (begin
       (set! *ap-k* k^)
       (set! *v* (+ v^ *v*))
       (set! *pc* apply-k)))))

;;; We also need to pass an extra argument to empty-k, we
;;; pass a continuation to empty-k.
(define-union cont
  (make-sub1-k n^ k^)
  (make-sub2-k v^ k^)
  (empty-k jumpout))

(define tramp
  (λ ()
    (begin
      (*pc*)
      (tramp))))

(require racket/trace)
(trace tramp)

;;; let/cc is from Racket.
(let/cc jumpout
  (begin
    (set! *k* (cont_empty-k jumpout))
    (set! *n* 5)
    (set! *pc* fib)
    (tramp)))
