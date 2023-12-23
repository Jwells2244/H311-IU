#lang racket
(require racket/trace)


(require "parenthec.rkt")
(define *n* #f)
(define *k* #f)
(define *ap-k* #f)
(define *v* #f)

(define factorial
  (λ ()
    (cond
      [(zero? *n*)
       (begin
         (set! *ap-k* *k*)
         (set! *v* 1)
         (apply-k))]
      [else
       (begin (set! *k* (cont_make-mult-k *n* *k*))
              (set! *n* (sub1 *n*))
              (factorial))])))


;;; We replace match with a union-case. union-case takes a
;;; value and a union name, and matches the value with each
;;; case of the union. Here don't need to use quasiquote,
;;; comma, and the "<union>_" in the union pattern.
(define apply-k
 (λ ()
   (union-case *ap-k* cont
     [(empty-k) *v*]
     [(make-mult-k v^ k^)
      (begin
        (set! *ap-k* k^)
        (set! *v* (* v^ *v*))
        (apply-k))])))

;(define empty-k
;  (λ ()
;    `(empty-k)))
;(define make-mult-k
;  (λ (v^ k^)
;    `(make-mult-k ,v^ ,k^)))

;;; We replace helper functions with define-union.  When we
;;; invoke those helper functions, we add a "<union>_"
;;; before each function name, where <union> is the union
;;; name, for example, cont here.
(define-union cont
  (make-mult-k v^ k^)
  (empty-k))


(begin
  (set! *k* (cont_empty-k))
  (set! *n* 5)
  (factorial))
