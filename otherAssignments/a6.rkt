#lang racket
(require racket/trace)
(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only
          (error 'empty-k "You can only invoke the empty continuation once")
          (begin (set! once-only #t) v))))))

(define empty-k2
  (lambda ()
    (lambda (v) v)))


;;1
(define binary-to-decimal
  (lambda (n)
    (cond
      [(null? n) 0]
      [else (+ (car n) (* 2 (binary-to-decimal (cdr n))))])))


(define binary-to-decimal-cps
  (lambda (n k)
    (cond
      [(null? n) (k 0)]
      [else (binary-to-decimal-cps (cdr n) (lambda (v) (k (+ (car n) (* 2 v)))))])))

;;(trace binary-to-decimal-cps)
;(binary-to-decimal '())
;(binary-to-decimal-cps '() (λ (x) x))
;(binary-to-decimal '(1))
;(binary-to-decimal-cps '(1) (λ (x) x))
;(binary-to-decimal '(0 1))
;(binary-to-decimal-cps '(0 1) (λ (x) x))
;(binary-to-decimal '(1 1 0 1))
;(binary-to-decimal-cps '(1 1 0 1) (λ (x) x))


;;2
(define star
  (lambda (m)
    (lambda (n)
      (* m n))))

(define star-cps
  (lambda (m k)
    (k (lambda (n k^)
      (k^ (* m n))))))
;(trace star)
;(trace star-cps)
;((star 2) 3)
;((star-cps 2 (λ (x) x)) 3 (lambda (x) x))
;((star ((star 2) 3)) 5)
;((star-cps ((star-cps 2 (λ (x) x)) 3 (λ (x) x)) (lambda (x) x)) 5 (lambda (x) x))


;;3
(define times
  (lambda (ls)
    (cond
      [(null? ls) 1]
      [(zero? (car ls)) 0]
      [else (* (car ls) (times (cdr ls)))])))

(define times-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) (k 0)]
      [else (times-cps (cdr ls) (lambda (v) (k (* (car ls) v))))])))

;(trace times)
;(trace times-cps)
;(times '(1 2 3 4 5))
;(times-cps '(1 2 3 4 5) (λ (x) x))
;(times '(1 2 3 0 3))
;;(times-cps '(1 2 3 0 3) (λ (x) x))
;;4
(define times-cps-shortcut
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) 0]
      [else (times-cps-shortcut (cdr ls) (lambda (v) (k (* (car ls) v))))])))


;(trace times-cps-shortcut)
;(times-cps-shortcut '(1 2 3 4 5) (λ (x) x))
;(times-cps-shortcut '(1 2 3 0 3) (λ (x) x))

;;5 
(define remv-first-9*
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(pair? (car ls))
       (cond
         [(equal? (car ls) (remv-first-9* (car ls)))
          (cons (car ls) (remv-first-9* (cdr ls)))]
         [else (cons (remv-first-9* (car ls)) (cdr ls))])]
      [(eqv? (car ls) '9) (cdr ls)]
      [else (cons (car ls) (remv-first-9* (cdr ls)))])))

(define remv-first-9*-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k '())]
      [(pair? (car ls))
         (remv-first-9*-cps (car ls)
                             (λ (v)
                               (if (equal? (car ls) v)
                                   (remv-first-9*-cps (cdr ls)
                                                      (λ (v^)
                                                        (k (cons (car ls) v^))))
                                   (remv-first-9*-cps (cdr ls)
                                                      (λ (v^)
                                                        (k (cons v (cdr ls))))))))]
         [(eqv? (car ls) '9) (k (cdr ls))]
      [else (remv-first-9*-cps (cdr ls) (lambda (v) (k (cons (car ls) v))))])))


;(trace remv-first-9*-cps)
;(remv-first-9* '((1 2 (3) 9)))
;(remv-first-9*-cps '((1 2 (3) 9)) (lambda (x) x))
;(remv-first-9* '((1 9)))
;(remv-first-9*-cps '((1 9)) (lambda (x) x))
;(remv-first-9* '((1) 9))
;(remv-first-9*-cps '((1) 9) (lambda (x) x))
;(remv-first-9* '((1) (9)))
;(remv-first-9*-cps '((1) (9)) (lambda (x) x))
;(remv-first-9* '(9 (9 (9 (9)))))
;(remv-first-9*-cps '(9 (9 (9 (9)))) (lambda (x) x))
;(remv-first-9* '(((((9) 9) 9) 9) 9))
;(remv-first-9*-cps '(((((9) 9) 9) 9) 9) (lambda (x) x))

;;6
(define cons-cell-count
  (lambda (ls)
    (cond
      [(pair? ls)
       (add1 (+ (cons-cell-count (car ls)) (cons-cell-count (cdr ls))))]
      [else 0])))

(define cons-cell-count-cps
  (lambda (ls k)
    (cond
      [(pair? ls)
       (cons-cell-count-cps (car ls) (lambda (v) (cons-cell-count-cps (cdr ls) (lambda (w) (k (add1 (+ v w)))))))]
      [else (k 0)])))

;(trace cons-cell-count-cps)
;(cons-cell-count '(1 2 3 4))
;(cons-cell-count-cps '(1 2 3 4) (lambda (x) x))
;(cons-cell-count '(1 2 (3 (4) 5) 4 ()))
;(cons-cell-count-cps '(1 2 (3 (4) 5) 4 ()) (lambda (x) x))


;;7
(define find
  (lambda (u s)
    (let ((pr (assv u s)))
      (if pr (find (cdr pr) s) u))))

(define find-cps
  (lambda (u s k)
    (let ((pr (assv u s)))
      (if pr (find-cps (cdr pr) s (lambda (v) (k v))) (k u)))))

;(trace find)
;(trace find-cps)

;(find 5 '((5 . a) (6 . b) (7 . c)))
;(find-cps 5 '((5 . a) (6 . b) (7 . c)) (lambda (x) x))
;(find 7 '((5 . a) (6 . 5) (7 . 6)))
;(find-cps 7 '((5 . a) (6 . 5) (7 . 6)) (lambda (x) x))
;(find 5 '((5 . 6) (9 . 6) (2 . 9)))
;(find-cps 5 '((5 . 6) (9 . 6) (2 . 9)) (lambda (x) x))

;;8
(define ack
  (lambda (m n)
    (cond
      [(zero? m) (add1 n)]
      [(zero? n) (ack (sub1 m) 1)]
      [else (ack (sub1 m)
                 (ack m (sub1 n)))])))
(define ack-cps
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack-cps (sub1 m) 1 k)]
      [else
       (ack-cps m
                (sub1 n)
                (lambda (v)
                  (ack-cps (sub1 m) v k)))])))
;(trace ack-cps)
;(ack 1 2)
;(ack-cps 1 2 (lambda (x) x))
;(ack 2 2)
;(ack-cps 2 2 (lambda (x) x))

;;9
(define fib
  (lambda (n)
    ((lambda (fib)
       (fib fib n))
     (lambda (fib n)
       (cond
     [(zero? n) 0]
     [(zero? (sub1 n)) 1]
     [else (+ (fib fib (sub1 n)) (fib fib (sub1 (sub1 n))))])))))
(define fib-cps
  (lambda (n k)
    ((lambda (fib-cps k^)
       (fib-cps fib-cps n k^))
     (lambda (fib-cps n k^^)
       (cond
         [(zero? n) (k^^ 0)]
         [(zero? (sub1 n)) (k^^ 1)]
         [else (fib-cps fib-cps (sub1 n)
                        (lambda (v)
                          (fib-cps fib-cps (sub1 (sub1 n))
                                   (lambda (w)
                                     (k^^ (+ v w))))))]))
     k)))
       

;10
(define null?-cps
    (lambda (ls k)
      (k (null? ls))))
(define car-cps
    (lambda (pr k)
      (k (car pr))))
(define cdr-cps
    (lambda (pr k)
      (k (cdr pr))))
(define unfold
  (lambda (p f g seed)
    ((lambda (h)
       ((h h) seed '()))
     (lambda (h)
       (lambda (seed ans)
     (if (p seed)
         ans
         ((h h) (g seed) (cons (f seed) ans))))))))
(define unfold-cps
  (lambda (p f g seed k)
    ((lambda (h k2)
       (h h (lambda (v)
                (v seed '() k2))))
     (lambda (h k3)
       (k3 (lambda (seed ans k4)
             (p seed (λ (q)
                       (if q
                           (k4 ans)
                           (h h (λ (h^)
                                  (g seed (λ (g^)
                                            (f seed (λ (f^)
                                                      (h^ g^ (cons f^ ans)k4))))))
                              ))))))) k)))
;(trace unfold-cps)
;(unfold null? car cdr '(a b c d e))
;(unfold-cps null?-cps car-cps cdr-cps '(a b c d e) (empty-k))

;11
(define empty-s
  (lambda ()
    '()))

(define unify
  (lambda (u v s)
    (cond
      ((eqv? u v) s)
      ((number? u) (cons (cons u v) s))
      ((number? v) (unify v u s))
      ((pair? u)
       (if (pair? v)
       (let ((s (unify (find (car u) s) (find (car v) s) s)))
             (if s (unify (find (cdr u) s) (find (cdr v) s) s) #f))
       #f))
      (else #f))))
(define unify-cps
  (lambda (u v s k)
    (cond
      ((eqv? u v) (k s))
      ((number? u) (k (cons (cons u v) s)))
      ((number? v) (unify-cps v u s k))
      ((pair? u)
       (if (pair? v)
           (find-cps (car u) s
                     (lambda (v1)
                       (find-cps (car v) s
                                 (lambda (w)
                                   (unify-cps v1 w s (lambda (q)
                                                       (if q
                                                           (find-cps (cdr u) s (lambda (j)
                                                                                 (find-cps (cdr v) s (lambda (p)
                                                                                                       (unify-cps j p q k)))))
                                                           (k #f))))))))
           (k #f)))
      (else (k #f)))))
                                             
;(trace unify-cps)
;(unify 'x 5 (empty-s))
;(unify-cps 'x 5 (empty-s) (lambda (x) x))
;(unify 'x 5 (unify 'y 6 (empty-s)))
;(unify-cps 'x 5 (unify-cps 'y 6 (empty-s) (lambda (x) x)) (lambda (y) y))
;;(unify '(x y) '(5 6) (empty-s))
;;(unify-cps '(x y) '(5 6) (empty-s) (lambda (x) x))
;(unify 'x 5 (unify 'x 6 (empty-s)))
;(unify-cps 'x 5 (unify-cps 'x 6 (empty-s) (lambda (x) x)) (lambda (y) y))
;(unify '(x x) '(5 6) (empty-s))
;(unify-cps '(x x) '(5 6) (empty-s) (lambda (x) x))
;(unify '(1 2 3) '(x 1 2) (empty-s))
;(unify-cps '(1 2 3) '(x 1 2) (empty-s) (lambda (x) x))
;(unify 'x 'y (empty-s))
;(unify-cps 'x 'y (empty-s) (lambda (x) x))


;12
(define M
  (lambda (f)
    (lambda (ls)
      (cond
        ((null? ls) '())
        (else (cons (f (car ls)) ((M f) (cdr ls))))))))
(define M-cps
  (lambda (f k)
    (k (lambda (ls k)
      (cond
        ((null? ls) (k '()))
        (else (M-cps f (lambda (v)
                         (f (car ls) (lambda (w)
                                       (v (cdr ls) (lambda (q)
                                                     (k (cons w q))))))))))))))
;(trace M-cps)
;(trace fib-cps)
;((M-cps fib-cps (empty-k)) '(1 2 3 4 5) (empty-k))

;13
(define add1-cps
  (lambda (x k)
    (k (+ 1 x))))
(define use-of-M
  ((M (lambda (n) (add1 n))) '(1 2 3 4 5)))
(define use-of-M-cps
  ((M-cps (lambda (n k) (k (add1 n))) (empty-k)) '(1 2 3 4 5) (empty-k)))
use-of-M-cps

;;Brainteasers
;;14
(define strange
  (lambda (x)
    ((lambda (g) (lambda (x) (g g)))
     (lambda (g) (lambda (x) (g g))))))
(define strange-cps
  (lambda (x k)
    ((lambda (g k)
       (k (lambda (x k^) (g g k^))))
     (lambda (g k)
       (k (lambda (x k^) (g g k^))))
     k)))

;15
(define use-of-strange
  (let ([strange^ (((strange 5) 6) 7)])
    (((strange^ 8) 9) 10)))
(define use-of-strange-cps
  (let ([strange^ (strange-cps 5
                               (λ (v)
                                 (v 6
                                    (λ (v^)
                                      (v 7 (empty-k))))))])
    (strange^ 8
               (λ (v)
                 (v 9
                    (λ (v^)
                      (v 10 (empty-k))))))))

;16
(define why
  (lambda (f)
    ((lambda (g)
       (f (lambda (x) ((g g) x))))
     (lambda (g)
       (f (lambda (x) ((g g) x)))))))

(define why-cps
  (lambda (f k)
    ((lambda (g k)
       (f (lambda (x k)
               (g g (lambda (v) (v x k))))k))
     (lambda (g k)
       (f (lambda (x k)
               (g g (lambda (v) (v x k))))k))k)))

(define almost-length
    (lambda (f)
      (lambda (ls)
        (if (null? ls)
            0
            (add1 (f (cdr ls)))))))


;17

(define why-cps-cps
  (lambda (f k c)
    ((lambda (g k c^)
       (f (lambda (x k c^^)
               (g g (lambda (v) (v x k c^^))))k c^))
     (lambda (g k c^)
       (f (lambda (x k c^^)
               (g g (lambda (v) (v x k c^^))))k c^))k c)))
