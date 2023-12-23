#lang racket
(require racket/trace)

;;This is just the normal length function cpsd
;Reverse engineering
(define length
  (λ (ls)
    (cond
      ((null? ls) 0)
      (else (add1 (length (cdr ls)))))))
(length '(a b c d e))
(define length-cps
  (λ (ls k)
    (cond
      ((null? ls) (k 0))
      (else (length-cps (cdr ls)
                        (λ (v)
                          (k (add1 v))))))))

;; (trace length-cps)
;; (length-cps '(a b c d e) (λ (v) v))
;This is length cps, cpsd again. Why you would exactly do this, I don't know but whatever I'm sure we're going
;to be tested on this in the exam Thursday.
;;Basically from what I can tell, to cps something again you add another continuation, this time call it c, and where you apply k to the result, you
;apply c as well. And when you extend the environment with the lambda (v), you have to add in another c variable too, maybe c^. In addition,
;in this example, the result of the else case is applied to this c. Cpsing it again makes it more concise?
(define length-cps²
  (λ (ls k c)
    (cond
      ((null? ls) (k 0 c))
      (else (length-cps² (cdr ls);first parameter
                         (λ (v c^) ;second parameter
                           (k (add1 v) c^))
                         c);third parameter
            ))))


;;In addition, in the call, for the first continuation that you give it, you give it the (λ (v k) (k v)), instead of just (empty-k)
;;But I dont know why, just try to remember it. When you cps something twice and you call it, you have to add a second variable to the beginning of the
;identity function, and then flip them around and apply them in the body of the lambda, and also have your identity function as the third argument,
;in this case c
;;(trace length-cps²)
;; (length-cps² '(a b c d e)
;;              (λ (v k)
;;                (k v))
;;              (λ (v) v))

(define fib-memo
  (let ((memo '((0 . 1) (1 . 1))))
    (λ (n)
      (cond
        ((assv n memo)
         =>
         (λ (p)
           (cdr p)))
        (else
         (let ((rst (+ (fib-memo (sub1 n))
                       (fib-memo (sub2 n)))))
           (set! memo
                 (cons `(,n . ,rst) memo))
           rst))))))

(define sub2
  (λ (n) (sub1 (sub1 n))))

(define fib-memo-cps
  (let ((memo '((0 . 1) (1 . 1))))
    (λ (n k)
      (cond
        ((assv n memo)
         =>
         (λ (p)
           (k (cdr p))));applys k to the result because this (cdr p) is being returned.
        (else
         (fib-memo-cps (sub1 n)
                       (λ (v)
                         (fib-memo-cps (sub2 n)
                                       (λ (w)
                                         (let ((rst (+ v w)))
                                           (set! memo
                                                 (cons `(,n . ,rst) memo))
                                           (k rst)))))))))))

;; (time (fib-memo-cps 40000 (λ (v) v)))
;; (time (fib-memo 40000))
(printf "let/cc practice ~n")
(+ 9 (+ (+ 3 4) 5)) ;21 obv, because 7 + 5 + 9 equals 21

(+ 9 (+ (let/cc k (k (+ 3 4)));Now this is different because we are using let/cc, which stands for let current continuation
        5)) ;This means that as soon as k is found to be a value, it will jumpout, and go from there. In this case, it still equals 21
;because the let/cc found a value for k at (+ 3 4), meaning 7, it pulled it out, but it still found the + k 5 and the + 9 k statements, meaning
;it still equals 21. Stupid example.

(let/cc k (k (+ (k (+ 3 6)) 4))) ;This is just 9, because the k found a value, jumped out with the value, and did not care about adding 4 to that value.

(call/cc (λ (k)
           (k (+ 3 4)))) ;Here is call/cc, stands for call current continuation, and I believe it is the same as let/cc, except for it takes one argument, which is a
;lambda expression where the variable bounded by the expressionn is what is is looking for, rather than let/cc var (expression), we can do call/cc (lambda expression)
;in this case, the call/cc also makes the k evaluate as 7, because it gets the value and jumps out again.
(+ 9 (+ (call/cc (λ (k) (k (+ 3 4)))) ;This becomes 21 again, I wish they would've given us better examples, because it functions the same way as let/cc did
        5)) ;for this example, where it found that k=7, it jumped out and then did the other two additions

(+ 9 (let/cc k (k (k (k (+ (k 4) (k 3))))))) ;This is where it gets interesting. So the let/cc finds the value for k and it jumps out to the + 9 k argument
;Except you may think this evaluates to 16. It doesn't. It instead evaluates to 13, because it finds that (k 4) block, realizes k is equal to 4, realizes that we
;have found the value for k, and jumps out to add it with 9, giving 13.

(let/cc k (+ (k k) (k 4))) ;This gives us a procedure, because the value of k is k applied to k, which is a procedure, and not an actual value.

(let ((c (let/cc k (+ (k k) (k 4))))) ;This goes into the false case, and then the true case, because c is first a number, and then when it gets called
  (if (number? c)  ;in the body of the true case, it then becomes a procedure in that let case? I'm not really sure about this one.
      (begin
        (printf "In true case\n")
        c)
      (begin
        (printf "In false case\n")
        (c 5))))

;; (let ((c ...))
;;   (if (number? c)
;;       (begin
;;         (printf "In true case\n")
;;         c)
;;       (begin
;;         (printf "In false case\n")
;;         (c 5))))


(let ((n 5) ;This sets n 5, r 1, and is doing a let/cc where k is equal to a procedure. 
      (r 1));And c is set to this procedure. 
  (let ((c (let/cc k (k k))))
    (if (zero? n)
        r ;Checks if n is equal to zero, its not, so goes into this begin. Sets r to be * 5 1, so 5, and sets n to 4, and calls c again? Idk about that one.
        (begin ;now n 4, r 5, c is equal to this procedure again. now r is 20, n is 3, r is 60, n is 2, n is 120, n is 1, r is 240, n is 0, and finally the r gets returned
          (set! r (* n r));as 120
          (set! n (sub1 n))
          (c c)))))

(define value-of
  (λ (e env)
    (match e
      (`,n #:when (number? n) n)
      (`,y #:when (symbol? y) (env y))
      (`(λ (,var) ,body)
       #:when (symbol? var)
       (λ (arg)
         (value-of body (λ (y)
                          (cond
                            ((eq? y var) arg)
                            (else (env y)))))))
      (`(,rator ,rand)
       ((value-of rator env) (value-of rand env))))))

(trace value-of)
(value-of '(λ (a) 5) (λ (y) (error "Not found!")))
(value-of '((λ (a) a) 5) (λ (y) (error "Not found!")))

(define value-of-cps
  (λ (e env k)
    (match e
      (`,n #:when (number? n) (k n))
      (`,y #:when (symbol? y) (env y k))
      (`(sub1 ,n-exp) (sub1 n-exp))
      (`(λ (,var) ,body)
       #:when (symbol? var)
       (k (λ (arg k^^)
            (value-of-cps body
                          (λ (y k^)
                            (cond
                              ((eqv? y var) (k^ arg))
                              (else (env y k^))))
                          k^^))))
      (`(,rator ,rand)
       (value-of-cps rator
                     env
                     (λ (clos)
                       (value-of-cps rand
                                     env
                                     (λ (v)
                                       (clos v k)))))))))

;;This is our cpsd interpreter.
(trace value-of-cps)
(value-of-cps '(λ (a) 5)
              (λ (y) (error "Not found!"))
              (λ (v) v))
(value-of-cps '((λ (a) a) 5)
              (λ (y) (error "Not found!"))
              (λ (v) v))
;(value-of-cps '((λ (a) (sub1 a)) 5)
;              (λ (y) (error "Not found!"))
;              (λ (v) v))
