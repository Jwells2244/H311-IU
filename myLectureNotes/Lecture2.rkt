#lang racket

(define add
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (add n (sub1 m)))))))

(add 2 3)
(add 2 0)
(define mult
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (add n (mult n (sub1 m)))))))

(mult 3 5)

(define expt
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (mult n (expt n (sub1 m)))))))

(expt 2 4)

(define tetr
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (expt n (tetr n (sub1 m)))))))

(tetr 2 2)

(define G
  (lambda (i)
   (cond
     ((zero? i)
      (lambda (n m)
        (cond
          ((zero? m) n)
          (else (add1 ((G 0) n (sub1 m)))))))
     ((zero? (sub1 i))
      (lambda (n m)
        (cond
      ((zero? m) 0)
      (else ((G 0) n ((G 1) n (sub1 m)))))))
      (else
        (lambda (n m)
          (cond
            ((zero? m) 1)
            (else ((G (sub1 i)) n ((G i) n (sub1 m))))))))))

((G 0) 4 5)
((G 1) 6 5)
((G 2) 2 4)
;;Lambda can by typed with Command backslash λ
(define add^ (λ (n m) ((G 0) n m)))
(define mult^ (λ (n m) ((G 1) n m)))
(define expt^ (λ (n m) ((G 2) n m)))
(define tetr^ (λ (n m) ((G 3) n m)))

(add^ 5 8)
(mult^ 5 8)
(expt^ 5 3)
(tetr^ 2 3)

;;Simplified version of the generator
(define G2
  (λ (p)
    (λ (n m)
      (cond
        ((zero? m)
         (cond
           ((zero? p) n)
           ((zero? (sub1 p)) 0)
           (else 1)))
        ((zero? p) (add1 ((G 0) n (sub1 m))))
        (else ((G (sub1 p)) n ((G p) n (sub1 m))))))))

