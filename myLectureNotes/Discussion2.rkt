#lang racket
(define G
  (λ (i)
    (cond
      ((zero? i)
       (λ (n m)
         (cond
           ((zero? m) n)
           (else (add1 ((G 0) n (sub1 m)))))))
      ((zero? (sub1 i))
       (λ (n m)
         (cond
           ((zero? m) 0)
           (else ((G 0) n ((G 1) n (sub1 m)))))))
      (else
       (λ (n m)
         (cond
           ((zero? m) 1)
           (else
            ((G (sub1 i)) n ((G i) n (sub1 m))))))))))

;(define G-1
;  (λ (i)
;    (λ (n m)
;    (cond
;      ((zero? i)
;         (cond
;           ((zero? m) n)
;           (else (add1 ((G 0) n (sub1 m)))))))
;      ((zero? (sub1 i))
;         (cond
;           ((zero? m) 0)
;           (else ((G 0) n ((G 1) n (sub1 m)))))))
;      (else
;         (cond
;           ((zero? m) 1)
;           (else
;            ((G (sub1 i)) n ((G i) n (sub1 m))))))))

(match `a ;;True
  (`,a #t)
  (`a #f)
  (else #f))

(match `a
  (`,a #:when (number? a) #t) ;;False
  (`a #f)
  (else #f))

;;'(a b) =>
;;'(a . (b . ())) (b)
(match '(a b)  ;;'(b)
  (`(,a . ,b) b)
  (`,a #f))
(match (λ (a) a) ;;False
  ('(λ (a) a) #t)
  (else #f))
;(match (λ (a) a)
;  ((λ (a) a) #t)
;  (else #f))

(match (λ (a) a) ;;True
  (`,a #t)
  (else #f))

(match ((λ (a) a) 'a) ;;True
  (`,a #t)
  ('a #f))

(match '(a c) ;;Without the period in the ` list, the match will just return the element. With the period,
  (`(,a . ,b) b) ;;the function will return '(c)
  (else #f))
(match '(a b c) ;;With a dot this return '(b c). With the dot, the function will basically return the cdr,
  (`(,a . ,b) b) ;;Without the dot it is just false, because the list is a different size
  (else #f))


(define save-the
  (λ (pred l)
    (match l
      ('() '())
      (`(,a . ,d)
       (cond
         ((pred a)
          (cons a (save-the pred d))) ;;Basically checks if the predicate is true with the car, and if it is, goes
         (else  ;;to the cdr with the even number saved
          (save-the pred d))))))) ;;Otherwise just calls the cdr if the number isnt even
(save-the even? '(1 2 3 4 5 6))

(define free? ;;Function from lecture 4
  (λ (var expr)
    (match expr
      (`,y
       #:when (symbol? y)
       (eqv? var y))
      (`(λ (,x) ,body)
       #:when (symbol? x)
       (and (free? var body)
            (not (eqv? x var))))
      (`(,rator ,rand)
       (or (free? var rator) (free? var rand))))))

(define save-the2
  (λ (pred l)
    (match l
      ('() '())
      (`(,a . ,d) #:when (pred a)
                  (cons a (save-the2 pred d)));;This function can be shrunk greatly with match, getting rid of cons
      (`(,_ . ,d)
           (save-the2 pred d)))))

(save-the2 even? '(1 2 3 4 5 6))

'((λ (x) ;;Rator, if you imagine this whole expression to be a box, this λ thing is the operator, and the a is the operand
    (x ;;x is not free
     (λ (y)
       (x y)))) ;;Both x and y are also not free
  a) ;;rand, and a free variable



