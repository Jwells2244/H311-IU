;;; We comment out the following two lines.
;;; #lang racket
;;; (require "parenthec.rkt")

(define-registers *n* *k* *ap-k* *v*)
(define-program-counter *pc*)

(define-label fib
  (cond
    ((<= *n* 1)
     (begin
       (set! *ap-k* *k*)
       (set! *v* 1)
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

(define-union cont
  (make-sub1-k n^ k^)
  (make-sub2-k v^ k^)
  (empty-k jumpout))

(define-label main
  (begin
    (set! *n* 5)
    (set! *pc* fib)
    (mount-trampoline cont_empty-k *k* *pc*)
    (printf "Value of (fib 5) is ~a\n" *v*)))

;;; We also comment out the invocation of main.
;;; (main)