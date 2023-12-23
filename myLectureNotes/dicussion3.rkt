#lang racket
;(define insertion-sort
;  (λ (ls)
;    (cond
;      ((empty? ls) ls)
;      (else (insert-sort-aux (car ls) (insertion-sort (cdr ls)))))))
;
;
;
;(define insert-sort-aux
;  (λ (x ls)
;    (cond
;      ((empty? ls) x)
;      ((empty? x ) ls)
;      ((< (car ls) x) (cons (car ls) (insert-sort-aux x (cdr ls))))
;    (else (cons x ls)))))
;
;(insertion-sort '(9 22 2 274 23 394 42 488 0 873 1))

(define takef
  (λ (pred)
    (λ (ls)
      (cond
      ((empty? ls) ls)
      ((pred (car ls)) (cons (car ls) ((takef pred) (cdr ls))))
      (else ((takef pred) (cdr ls)))))))


((takef number?) '(1 2 3 a b c d 5))

;;Do reverse with just car, cdr, cons, lambdas etc. Nothing fancy 
