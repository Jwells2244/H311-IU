#lang racket

(define add ;;Add function from lecture 1
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (add n (sub1 m)))))))

(add 2 3)
(add 3 5)

(define mult ;;Multiply function from lecture 1
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (add n (mult n (sub1 m)))))))

(mult 7 6)

(define expt ;;New exponential function, takes in 2 numbers 
  (lambda (n m)
    (cond
      ((zero? m) 1) ;;If the m is zero, returns 1, not 0 because we are multiplying
      (else (mult n (expt n (sub1 m))))))) ;;If m isnt 0, returns a recursive call taking 1 away from m, multiplying n by the result

(expt 2 3)
(expt 4 3)

(define tetr ;;Returns the n to the m to the m to the m and so on
  (lambda (n m)
    (cond
      ((zero? m) 1) ;;If the m is zero, returns 1
      (else (expt n (tetr n (sub1 m))))))) ;;Else, returns the tetr of n sub1 m times the exponential of n and the results

(tetr 2 3)
;; (tetr 3 5)

(define G ;;Math interpreter
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

(define add^ (λ (n m)  ((G 0) n m)))
(define mult^ (λ (n m) ((G 1) n m)))
(define expt^ (λ (n m) ((G 2) n m)))
(define tetr^ (λ (n m) ((G 3) n m)))

;; (add^ 5 8)
;; (mult^ 9 5)
;; (expt^ 3 5)
;; (tetr^ 2 4)

;; (define G^
;;   (λ (i)
;;     (λ (n m)
;;       (cond
;;         ((zero? m)
;;          (cond
;;            ((zero? i) n)
;;            ((zero? (sub1 i)) 0)
;;            (else 1)))
;;         ((zero? i) (add1 ((G^ 0) n (sub1 m))))
;;         (else ((G^ (sub1 i)) n ((G^ i) n (sub1 m))))))))

(define rember8 ;;Same remember 8 as lecture 1?
  (λ (ls)
    (cond
      ((empty? ls) empty)
      ((eqv? (car ls) 8) (rember8 (cdr ls)))
      (else (cons (car ls) (rember8 (cdr ls)))))))
