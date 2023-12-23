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
       ;; We also replace function invocation with set *pc*.
       (set! *pc* apply-k)))
    (else
     (begin
       (set! *k* (cont_make-sub1-k *n* *k*))
       (set! *n* (sub1 *n*))
       (set! *pc* fib)))))

(define-label apply-k
  (union-case *ap-k* cont
    ((empty-k jumpout) (dismount-trampoline jumpout))
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

;;; We don't need to define this tramp anymore.
#;
(define tramp
  (λ ()
    (begin
      (*pc*)
      (tramp))))

;;; We make a main function first with define-label.  In
;;; main, instead of setting *k* with let/cc, we use
;;; mount-trampoline from parenthec. Also, we don't need to
;;; invoke tramp anymore, because that is performed by
;;; mount-trampoline.
(define-label main
  (begin
    (set! *n* 5)
    (set! *pc* fib)
    (mount-trampoline cont_empty-k *k* *pc*)
    (printf "Value of (fib 5) is ~a\n" *v*)))

;;; Then we invoke main.
(main)
