#lang racket
(require racket/trace)
(require "monads.rkt")
;; Monads à La Mode
;; http://cswords.com/paper/alamode.pdf

(define length
  (λ (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))

(length '(1 2 3 4 5))

(define sum
  (λ (l)
    (cond
      ((null? l) 0)
      (else (+ (car l) (sum (cdr l)))))))

(sum '(1 2 3 4 5))

(define avg
  (λ (l)
    (/ (sum l) (length l))))

(avg '(1 2 3 4 5))
#|
(define avg-helper
  (λ (l)
    (cond
      ((null? l) '(0 . 0))
      (else
       (match (avg-helper (cdr l))
         (`(,len . ,sum)
          `(,(add1 len) . ,(+ (car l) sum))))))))

(define avg^
  (λ (l)
    (match (avg-helper l)
      (`(,len . ,sum) (/ sum len)))))

(avg^ '(1 2 3 4 5))
|#
;; values
(define avg-helper
  (λ (l)
    (cond
      ((null? l) (values 0 0))
      (else
       (let-values (((len sum) (avg-helper (cdr l))))
         (values (add1 len) (+ (car l) sum)))))))

(define avg^
  (λ (l)
    (let-values (((len sum) (avg-helper l)))
      (/ sum len))))


(avg^ '(1 2 3 4 5))

;; bind-state
;; (-> (State S A)
;;     (-> A (State S B))
;;   (State S B))
;; bind-state changes the pure value (e.g. A above) in the
;; State monad without touching the state S.

;; inj-state
;; (-> A
;;   (State S A))
;; inj-state packages a pure value in a monad.

;; run-state
;; (-> (State S A)
;;   (-> S (Pair A S)))
;; run-state takes a State monad and returns a function.
;; This function takes an initial state S and returns the
;; final value of pure value A and the state S as a cons
;; pair after letting the monadic computations run.

#;
(define avg-helper
  (λ (l)
    (cond
      ((null? l) '(0 . 0))
      (else
       (match (avg-helper (cdr l))
         (`(,len . ,sum)
          `(,(add1 len) . ,(+ (car l) sum))))))))

#|
(define avg-helper/monad
  (λ (l)
    (cond
      ((null? l) (inj-state 0))
      (else
       (bind-state
         (avg-helper/monad (cdr l))
         (λ (len)
           (bind-state
             (get)
             (λ (sum)
               (bind-state
                 (put (+ (car l) sum))
                 (λ (_)
                   (inj-state (add1 len))))))))))))

(define avg/monad
  (λ (l)
    (bind-state
      (avg-helper/monad l)
      (λ (len)
        (bind-state
          (get)
          (λ (sum)
            (inj-state (/ sum len))))))))

((run-state (avg/monad '(1 2 3 4 5))) 0)
|#

#|
(define avg-helper/monad
  (λ (l)
    (cond
      ((null? l) (inj-state 0))
      (else
       (go-on
         (len <- (avg-helper/monad (cdr l)))
         (sum <- (get))
         (put (+ (car l) sum))
         (inj-state (add1 len)))))))

(define avg/monad
  (λ (l)
    (go-on
      (len <- (avg-helper/monad l))
      (sum <- (get))
      (let ((avg (/ sum len)))
        (inj-state avg)))))

((run-state (avg-helper/monad '(1 2 3 4 5))) 0)
((run-state (avg/monad '(1 2 3 4 5))) 0)
|#


(define avg-helper/monad
  (λ (l)
    (cond
      ((null? l) (inj-state 0))
      (else
       (go-on
         (sum <- (avg-helper/monad (cdr l)))
         (len <- (get))
         (put (add1 len))
         (let ((sum (+ (car l) sum)))
           (inj-state sum)))))))

(define avg/monad
  (λ (l)
    (go-on
      (sum <- (avg-helper/monad l))
      (len <- (get))
      (let ((avg (/ sum len)))
        (inj-state avg)))))

((run-state (avg-helper/monad '(1 2 3 4 5))) 0)
((run-state (avg/monad '(1 2 3 4 5))) 0)

(define sub2
  (λ (n)
    (sub1 (sub1 n))))

;; This store-passing-style memoized fibonacci function can
;; be rewritten using the State monad where memo is the
;; store and the fibonacci result is the pure value.
(define fib-memo
  (λ (n memo)
    (cond
      ((assv n memo)
       =>
       (λ (pr)
         `(,(cdr pr) . ,memo)))
      (else
       (let*
           ((res-sub1 (fib-memo (sub1 n) memo))
            (fib-sub1 (car res-sub1))
            (memo^ (cdr res-sub1))
            (res-sub2 (fib-memo (sub2 n) memo^))
            (fib-sub2 (car res-sub2))
            (memo^^ (cdr res-sub2)))
         (let ((result (+ fib-sub1 fib-sub2)))
           `(,result . ((,n . ,result) . ,memo^^))))))))

(fib-memo 10 '((1 . 1) (0 . 1)))

#|
(define fib-memo/store
  (λ (n)
    (bind-state
      (get)
      (λ (memo)
        (cond
          ((assv n memo)
           =>
           (λ (pr)
             (let ((result (cdr pr)))
               (inj-state result))))
          (else
           (bind-state
             (fib-memo/store (sub1 n))
             (λ (fib-sub1)
               (bind-state
                 (fib-memo/store (sub2 n))
                 (λ (fib-sub2)
                   (let ((result (+ fib-sub1 fib-sub2)))
                     (bind-state
                       (get)
                       (λ (memo^^)
                         (bind-state
                           (put `((,n . ,result) . ,memo^^))
                           (λ (_)
                             (inj-state result))))))))))))))))

((run-state (fib-memo/store 10)) '((1 . 1) (0 . 1)))
|#

(define fib-memo/store
  (λ (n)
    (go-on
      (memo <- (get))
      (cond
        ((assv n memo)
         =>
         (λ (pr)
           (let ((result (cdr pr)))
             (inj-state result))))
        (else
         (go-on
           (fib-sub1 <- (fib-memo/store (sub1 n)))
           (fib-sub2 <- (fib-memo/store (sub2 n)))
           (assign (result <- (+ fib-sub1 fib-sub2)))
           (memo^^ <- (get))
           (put `((,n . ,result) . ,memo^^))
           (inj-state result)))))))

((run-state (fib-memo/store 10)) '((1 . 1) (0 . 1)))
((run-state (fib-memo/store 500)) '((1 . 1) (0 . 1)))
