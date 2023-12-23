#lang racket
(require "mk-func.rkt")

#;
(defrel (appendo ls s o)
  (disj
    (conj (== '() ls) (== s o))
    (fresh (a d res)
      (== `(,a . ,d) ls)
      (== `(,a . ,res) o)
      (appendo d s res))))

(define appendo
  (λ (l1 s2 o)
    (λ (S)
      (λ () ;; be sure to use the same trick here to make your definitons work!
        ((disj
           (conj (== '() l1) (== s2 o))
           (call/fresh 'a
             (λ (a)
               (call/fresh 'd
                 (λ (d)
                   (call/fresh 'res
                     (λ (res)
                       (conj
                         (== `(,a . ,d) l1)
                         (== `(,a . ,res) o)
                         (appendo d s2 res)))))))))
         S)))))



(define reverse
  (lambda (ls)
    (cond
      ((equal? '() ls) '())
      (else
       (match-let* ((`(,a . ,d) ls)
                    (res (reverse d)))
         (append res `(,a)))))))
#;
(defrel (reverseo ls o)
  (disj
   (conj
    (== '() ls)
    (== o '()))
  (fresh (a d res)
          (== `(,a . ,d) ls)
          (reverseo d res)
          (appendo res `(,a) o))))


;reverseo
(define reverseo
  (λ (ls o)
    (λ (S)
      (λ ()
        ((disj
          (conj (== '() ls) (== o '()))
          (call/fresh 'a
                      (λ (a)
                        (call/fresh 'd
                                    (λ (d)
                                      (call/fresh 'res
                                                  (λ (res)
                                                    (conj
                                                     (== `(,a . ,d) ls)
                                                     (reverseo d res)
                                                     (appendo res `(,a) o)))))))))
         S)))))
;(run*-g (λ (q) (reverseo '() q)))
;(run*-g (λ (q) (reverseo '(a) q)))
;(run*-g (λ (q) (reverseo '(a b c d) q)))
;(run*-g (λ (q) (call/fresh 'x (λ (x) (reverseo `(a b ,x c d) q)))))
;(run*-g (λ (x) (reverseo `(a b ,x d) '(d c b a))))
;(run*-g (λ (x)
;            (reverseo `(a b c d) `(d . ,x))))
;(run*-g
;    (λ (q)
;      (call/fresh 'x
;        (λ (x) (reverseo '(a b c d) `(d    ,q . ,x))))))
;(run-g 10
;    (λ (q)
;      (call/fresh 'x
;        (λ (x)
;          (call/fresh 'y
;            (λ (y)
;              (conj
;                (== `(,x ,y) q)
;                (reverseo x y))))))))


 
(define stutter
  (lambda (ls)
    (cond
      ((equal? '() ls) '())
      (else
        (match-let* ((`(,a . ,d) ls)
             (res (stutter d)))
          `(,a ,a . ,res))))))
#;
(defrel (stuttero ls o)
  (disj
   (conj
    (== '() ls)
    (== o '()))
   (fresh (a d res)
          (== `(,a . ,d) ls)
          (== `(,a ,a . ,res) o)
          (stuttero d res))))

(define stuttero
  (λ (ls o)
    (λ (S)
      (λ ()
        ((disj
          (conj (== '() ls) (== o '()))
          (call/fresh 'a
                      (λ (a)
                        (call/fresh 'd
                                    (λ (d)
                                      (call/fresh 'res
                                                  (λ (res)
                                                    (conj
                                                     (== `(,a . ,d) ls)
                                                     (== `(,a ,a . ,res) o)
                                                     (stuttero d res)))))))))
         S)))))
;(run-g 1 (λ (q) (stuttero q '(1 1 2 2 3 3))))
;(run*-g (λ (q) (stuttero q '(1 1 2 2 3 3))))
;(run-g 1 (λ (q)
;           (call/fresh 'a
;             (λ (a)
;               (call/fresh 'b
;                 (λ (b)
;                   (call/fresh 'c
;                     (λ (c)
;                       (call/fresh 'd
;                         (λ (d)
;                           (conj
;                             (== q `(,a ,b ,c ,d))
;                             (stuttero `(,b 1) `(,c . ,d)))))))))))))
;(run-g 1 (λ (q) h
;           (call/fresh 'a
;             (λ (a)
;               (call/fresh 'b
;                 (λ (b)
;                   (call/fresh 'c
;                     (λ (c)
;                       (call/fresh 'd
;                         (λ (d)
;                           (conj
;                             (== q `(,a ,b ,c ,d))
;                             (stuttero `(,b 1) `(,c . ,d)))))))))))))
;(run-g 1 (λ (q)
;             (call/fresh 'e
;               (λ (e)
;                 (call/fresh 'f
;                   (λ (f)
;                     (call/fresh 'g
;                       (λ (g)
;                         (conj
;                           (== q `(,e ,f ,g))
;                           (stuttero `(,e . ,f) g))))))))))
;(run-g 2 (λ (q)
;             (call/fresh 'e
;               (λ (e)
;                 (call/fresh 'f
;                   (λ (f)
;                     (call/fresh 'g
;                       (λ (g)
;                         (conj
;                           (== q `(,e ,f ,g))
;                           (stuttero `(,e . ,f) g))))))))))
