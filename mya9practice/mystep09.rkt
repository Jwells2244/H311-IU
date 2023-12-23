#lang racket
(require racket/trace)
(require "parenthec.rkt")

;;; We replace our global definitions with define-registers.
(define-registers *n* *k* *ap-k* *v*)
(define-program-counter *pc*)
;;; We add a program counter.

;;; We replace function definitions with define-label.
;; We also replace function invocation with set
       ;; *pc*.
(define-label factorial
    (cond
      [(zero? *n*)
       (begin
         (set! *ap-k* *k*)
         (set! *v* 1)
         (set! *pc* apply-k))]
      [else
       (begin (set! *k* (cont_make-mult-k *n* *k*))
              (set! *n* (sub1 *n*))
              (set! *pc* factorial))]))


(define-label apply-k
   (union-case *ap-k* cont
     [(empty-k jumpout) (jumpout *v*)]
     [(make-mult-k v^ k^)
      (begin
        (set! *ap-k* k^)
        (set! *v* (* v^ *v*))
        (set! *pc* apply-k))]))

;(define empty-k
;  (λ ()
;    `(empty-k)))
;(define make-mult-k
;  (λ (v^ k^)
;    `(make-mult-k ,v^ ,k^)))

;;; We also need to pass an extra argument to empty-k, we
;;; pass a continuation to empty-k.
(define-union cont
  (make-mult-k v^ k^)
  (empty-k jumpout))

(define tramp
  (λ ()
    (begin
      (*pc*)
      (tramp))))

;;; let/cc is from Racket.

(let/cc jumpout
  (begin
    (set! *k* (cont_empty-k jumpout))
    (set! *n* 5)
    (set! *pc* factorial)
    (tramp)))