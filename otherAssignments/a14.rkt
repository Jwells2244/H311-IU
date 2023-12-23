#lang pie
(claim step-zero?
  (-> Nat
    Atom))
(define step-zero?
  (λ (n-sub1)
    'false))

(claim zero?
  (-> Nat
      Atom))
(define zero?
  (λ (n)
    (which-Nat n 'true step-zero?)))

;1
(claim intriguing-word Atom)
(define intriguing-word 'this-is-an-intriguing-word-atom)

;intriguing-word

;2
(claim lucky-num Nat)
(define lucky-num 7)

;lucky-num

;3
(claim to-go-order (Pair Nat Atom))
(define to-go-order (cons 3 'Burger))

;to-go-order

;4 might need to make sure I've done this right
(claim MyFirstType U)
(define MyFirstType Nat)

;MyFirstType

;5
(claim my-thing-and-Atom (Pair MyFirstType U))
(define my-thing-and-Atom (cons 5 Nat))

;my-thing-and-Atom

;Past the intro stuff

;6
(claim with-Nats
  (-> (-> Nat Nat
          Nat)
      (Pair Nat Nat)
      Nat))

(define with-Nats
  (λ (n m)
      (n (car m) (cdr m))))
;(with-Nats (λ (n m) n) (cons 1 2))
;(with-Nats (λ (n m) (add1 m)) (cons 1 2))
;(check-same Nat (with-Nats (λ (n m) n) (cons 1 2)) 1)
;(check-same Nat (with-Nats (λ (n m) (add1 m)) (cons 1 2)) 3)


;7

(claim step-two?
  (-> Nat
      Atom))
(define step-two?
  (λ (sub1-n)
    't))
(claim at-least-two?
  (-> Nat
    Atom))
(define at-least-two?
  (λ (n)
    (which-Nat
      n
      'nil
      (λ (n-1)
        (which-Nat
          n-1
          'nil
          (λ (n-2)
                (step-two? n-2)))))))

;(check-same Atom (at-least-two? 0) 'nil)
;(check-same Atom (at-least-two? 1) 'nil)
;(check-same Atom (at-least-two? 41) 't)

;8
(claim + (-> Nat Nat
           Nat))
(define + (λ (n m) (rec-Nat n
                     m
                     (λ (k k+m) (add1 k+m)))))
 
(claim * (-> Nat Nat
           Nat))
(define * (λ (n m) (rec-Nat n
                     0
                     (λ (k k*m) (+ m k*m)))))

(claim expt (-> Nat Nat Nat))

(define expt
  (λ (n m)
    (rec-Nat
      m
      1
      (λ (k k^m) (* n k^m)))))

;(expt 4 3) ; 64
;(expt 2 6) ; 64
;(expt 3 3) ; 27


;9
(claim map
  (Π ((A U)
      (B U))
    (→ (→ A B) (List A)
       (List B))))

(define map
  (λ (A B v ls)
    (rec-List
      ls
      (the (List B) nil)
      (λ (carls cdrls prev)
                          (:: (v carls) prev)))))
;((map Nat Atom) zero? (:: 0 (:: 1 (:: 2 (:: 3 (:: 4 (:: 0 nil)))))))

;10 nth
(claim -cdr
  (Π ((A U))
    (-> (List A) (List A))))
(define -cdr
  (λ (A)
    (λ (a)
      (rec-List
        a
        (the (List A) nil)
        (λ (_ d _)
          d)))))

(claim first
  (Π ((A U))
    (→ (List A) A A)))
(define first
  (λ (A)
    (λ (es def)
      (rec-List es def (λ (a d r) a)))))

(claim nth
  (Π ((A U))
    (→ (List A) A Nat
       A)))

(define nth
  (λ (A)
    (λ (ls def n)
      (first
        A
        (iter-Nat
        n
        ls
        (-cdr A))
        def))))
;(nth Nat (:: 0 (:: 1 (:: 2 (:: 3 (:: 4 (:: 0 nil)))))) 20 2) ; (the Nat 2)

;11 Vectors
(claim vec-second
  (Π ((E U)
      (l Nat))
    (-> (Vec E (add1 (add1 l)))
        E)))
(define vec-second
  (λ (E l)
    (λ (es)
      (head (tail es)))))
(vec-second Nat 1 (vec:: 23 (vec:: 24 (vec:: 35 vecnil))))

;12 Max
(claim max (-> Nat Nat Nat))

(define max
  (lambda (x)
    (rec-Nat x (the (-> Nat Nat) (lambda (y) y))
      (lambda (x-1 res)
        (lambda (y)
          (add1 (rec-Nat y x-1
                  (lambda (y-1 _)
                    (res y-1)))))))))
;(max 2 3)
;(max 5 4)
;(max 3 3)

;13 ack
(claim ack (-> Nat Nat Nat))

(define ack
  (lambda (n)
    (rec-Nat n (the (-> Nat Nat) (λ (m) (add1 m)))
      (λ (n-1 r)
        (λ (m)
          (rec-Nat m (r 1) (λ (m-1 r2) (r r2))))))))

;(ack 2 2)
  
