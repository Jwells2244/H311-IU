#lang racket

;;Numbers
4
-34
0
;;Symbols
'4
'foo
'Hello 'my 'name 'is
(define foo 'bar)

;; let
(let ((v 5))v)

(let ((v 5)(x 10)(y 10))(+ x y))

;;Functions - lambda
(lambda (n)
  (+ 10 n))
(define add10
  (lambda (n)
    (+ 10 n)))
;;application
;;add10 5 ;no
;;(add10 5) ;correct syntax
;;(add10 10 20) Error of course

;;Booleans, True and false
#t
#f

;;Conditionals
;;(cond
 ;; (test1 expr1)
 ;; (test2 expr2)
  ;;...
  ;;(else else-expr))
;;Absolute value function
;;(define abs
;;  (lambda (n)
 ;;   (cond
  ;;    ((< n 0) (* n -1))
  ;;    (else n))))
;;abs(10)
;;abs(-10)
;;abs(0)
;;(add1 9)
;;(sub1 9)

(define add
  (lambda (m n)
    (cond
      ((zero? m) n)
      (else (add1 (add (sub1 m) n))))))
;;(add 5 10)

(define mult
  (lambda (m n)
  (cond
    ((zero? m) 0)
    (else (add (mult (sub1 m) n) n)))))
;;(mult 2 3)

;;Pairs
(define pair (cons 2 3))
;;car contents of address register
;;cdr contents of decrement register
;;Lists
(define list (cons 1 (cons 2 (cons 3 (cons 4 empty)))))
(define upto
  (lambda (n)
    (cond
      ((zero? n) empty)
    (else (cons n (upto (sub1 n)))))))
(upto 4)

;;Write a function that takes in a list with numbers inside that returns everything except
;;for 8s

(define rember8
  (lambda (ls)
    (cond
      ((empty? ls) empty)
      ((eqv? (car ls) 8) (rember8 (cdr ls)))
       (else (cons (car ls) (rember8 (cdr ls)))))))

(rember8 '(2 1 8 6 4 8))