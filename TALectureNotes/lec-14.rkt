#lang racket
(require racket/trace)
;; (define remainder
;;   (λ (m n)
;;     (cond
;;       ((< m n) m)
;;       (else
;;        (remainder (- m n) n)))))
;Quick remainder cps
(define remainder-cps
  (λ (m n k)
    (cond
      ((< m n) (k m))
      (else
       (remainder-cps (- m n) n (lambda (v) (k v)))))))
;(trace remainder-cps)
;(remainder-cps 5 4 (lambda (x) x))


;Trampolining lecture notes.
;; ;;; example 1: gcd
;; ;;; step 0
;; ;; (define gcd
;; ;;   (λ (a b)
;; ;;     (cond
;; ;;       ((zero? b) a)
;; ;;       (else (gcd b (remainder a b))))))

;; ;; (let ((a 12)
;; ;;       (b 24))
;; ;;   (gcd a b))

;; ;;; step 1: ANF, with a temporary register
;; ;;; Rename formals with surrounding "*"s
;; ;;; We introduce an *old-a* to save *a*'s current value,
;; ;;; which we will use later. If every recursive invocation
;; ;;; is a tail-call, we can ignore CPS steps.
;The star step is called making the function into a-normal form, where you rename the formals with stars,
;this is making the temporary registers. If every recursive invocation of the function is a tail-call, there is no reason to cps,
;because cps makes it tail-call.
;; (define gcd
;;   (λ (*a* *b*)
;;     (cond
;;       ((zero? *b*) *a*)
;;       (else
;;        (let* ((*old-a* *a*)
;;               (*a* *b*)
;;               (*b* (remainder *old-a* *b*)))
;;          (gcd *a* *b*))))))

;; (let* ((*a* 12)
;;        (*b* 24))
;;   (gcd *a* *b*))

;; ;;; step 2: registerize
;; (define *old-a* #f)
;; (define *a* #f)
;; (define *b* #f)

;; (define gcd
;;   (λ () ; *a* *b*
;;     (cond
;;       ((zero? *b*) *a*)
;;       (else
;;        (begin
;;          (set! *old-a* *a*)
;;          (set! *a* *b*)
;;          (set! *b* (remainder *old-a* *b*))
;;          (gcd))))))

;; (begin
;;   (set! *a* 12)
;;   (set! *b* 24)
;;   (gcd))

;; ;;; example 2: fib
;; ;;; step 0
(define sub2-cps
  (λ (n k)
    (sub1 (k (sub1 n)))))
;(sub2-cps 5 (lambda (v) v))

(define sub2
  (λ (n)
    (sub1 (sub1 n))))

;; (define fib
;;   (λ (n)
;;     (cond
;;       ((<= n 1) 1)
;;       (else (+ (fib (sub1 n))
;;                (fib (sub2 n)))))))

;; (fib 5)

;; ;;; step 1: CPS
;; (define fib-cps
;;   (λ (n k)
;;     (cond
;;       ((<= n 1) (k 1))
;;       (else (fib-cps (sub1 n)
;;              (λ (v)
;;                (fib-cps (sub2 n)
;;                 (λ (w)
;;                   (k (+ v w))))))))))

;; ;;; step 2: RI
;; (define fib-cps
;;   (λ (n k)
;;     (cond
;;       ((<= n 1) (apply-k k 1))
;;       (else
;;        (fib-cps (sub1 n) (make-sub1-k n k))))))

;; (define apply-k
;;   (λ (k v)
;;     (k v)))

;; (define empty-k
;;   (λ ()
;;     (λ (v) v)))

;; (define make-sub2-k
;;   (λ (v k)
;;     (λ (w)
;;       (apply-k k (+ v w)))))

;; (define make-sub1-k
;;   (λ (n k)
;;     (λ (v)
;;      (fib-cps
;;       (sub2 n)
;;       (make-sub2-k v k)))))


;;; step 3: data structure continuation and mix fib with
;;; insertL*
;; (define fib-cps
;;   (λ (n k)
;;     (cond
;;       ((<= n 1) (apply-k k 1))
;;       (else
;;        (fib-cps (sub1 n) (make-sub1-k n k))))))

;; (define apply-k
;;   (λ (k v)
;;     (match k
;;       (`(empty-k)
;;        (begin
;;          (printf "Empty-k should only be called once!~n")
;;          v))
;;       (`(make-sub2-k ,v^ ,k^)
;;        (apply-k k^ (+ v^ v)))
;;       (`(make-sub1-k ,n^ ,k^)
;;        (fib-cps (sub2 n^)
;;                 (make-sub2-k v k^)))

;;       (`(make-car-k ,insertL*-cdr^ ,k^)
;;        (apply-k k^ (cons v insertL*-cdr^)))
;;       (`(make-cdr-k ,q^ ,r^ ,ls^ ,k^)
;;        (let ((insertL*-cdr v))
;;          (cond
;;            ((pair? (car ls^))
;;             (insertL*-cps q^ r^ (car ls^)
;;                           (make-car-k insertL*-cdr k^)))
;;            ((eqv? (car ls^) q^)
;;             (apply-k k^
;;                      (cons r^ (cons (car ls^) insertL*-cdr))))
;;            (else
;;             (apply-k k^
;;                      (cons (car ls^) insertL*-cdr))))))

;;       (`(make-fib-k)
;;        (insertL*-cps 'x v '(a x (b x y z (x u)) j l z)
;;                      (empty-k))))))

;; (define empty-k
;;   (λ ()
;;     `(empty-k)))

;; (define make-sub2-k
;;   (λ (v^ k^)
;;     `(make-sub2-k ,v^ ,k^)))

;; (define make-sub1-k
;;   (λ (n^ k^)
;;     `(make-sub1-k ,n^ ,k^)))

;; (define insertL*-cps
;;   (λ (q r ls k)
;;     (cond
;;       ((null? ls) (apply-k k '()))
;;       (else
;;        (insertL*-cps q r (cdr ls)
;;         (make-cdr-k q r ls k))))))

;; (define make-car-k
;;   (λ (insertL*-cdr^ k^)
;;     `(make-car-k ,insertL*-cdr^ ,k^)))

;; (define make-cdr-k
;;   (λ (q^ r^ ls^ k^)
;;     `(make-cdr-k ,q^ ,r^ ,ls^ ,k^)))

;; (define make-fib-k
;;   (λ ()
;;     `(make-fib-k)))

;; step 4: trampoline
(define fib-cps
  (λ (n k)
    (cond
      ((<= n 1)
       (λ ()
         (apply-k k 1)))
      (else
       (λ ()
         (fib-cps (sub1 n) (make-sub1-k n k)))))))

(define apply-k
  (λ (k v)
    (match k
      (`(empty-k ,jumpout^)
       (λ ()
         (begin
           (printf
            "Empty-k should only be called once!~n")
           (jumpout^ v))))
      (`(make-sub2-k ,v^ ,k^)
       (λ ()
         (apply-k k^ (+ v^ v))))
      (`(make-sub1-k ,n^ ,k^)
       (λ ()
         (fib-cps (sub2 n^) (make-sub2-k v k^))))
      (`(make-car-k ,insertL*-cdr^ ,k^)
       (λ ()
         (apply-k k^ (cons v insertL*-cdr^))))
      (`(make-cdr-k ,q^ ,r^ ,ls^ ,k^)
       (let ((insertL*-cdr v))
         (cond
           ((pair? (car ls^))
            (λ ()
              (insertL*-cps q^ r^ (car ls^)
               (make-car-k insertL*-cdr k^))))
           ((eqv? (car ls^) q^)
            (λ ()
              (apply-k k^
               (cons r^ (cons (car ls^) insertL*-cdr)))))
           (else
            (λ ()
              (apply-k k^
               (cons (car ls^) insertL*-cdr)))))))
      (`(make-fib-k ,jumpout^)
       (λ ()
         (insertL*-cps 'x v '(a x (b x y z (x u)) j l z)
          (empty-k jumpout^)))))))

(define make-sub2-k
  (λ (v^ k^)
    `(make-sub2-k ,v^ ,k^)))

(define make-sub1-k
  (λ (n^ k^)
    `(make-sub1-k ,n^ ,k^)))

(define insertL*-cps
  (λ (q r ls k)
    (cond
      ((null? ls)
       (λ ()
         (apply-k k '())))
      (else
       (λ ()
         (insertL*-cps q r (cdr ls)
          (make-cdr-k q r ls k)))))))

(define make-car-k
  (λ (insertL*-cdr^ k^)
    `(make-car-k ,insertL*-cdr^ ,k^)))

(define make-cdr-k
  (λ (q^ r^ ls^ k^)
    `(make-cdr-k ,q^ ,r^ ,ls^ ,k^)))

(define make-fib-k
  (λ (jumpout^)
    `(make-fib-k ,jumpout^)))

(define tramp
  (λ (th)
    (tramp (th))))

(define empty-k
  (λ (jumpout^)
    `(empty-k ,jumpout^)))
;(trace tramp)
;(trace fib-cps)
;(let/cc jumpout
;  (tramp (fib-cps 5 (empty-k jumpout))))

;(let/cc jumpout
;  (tramp (fib-cps 5 (make-fib-k jumpout))))
;
(define bi-tramp
  (λ (th1 th2)
    (bi-tramp th2 (th1))))
;
(let/cc jumpout
  (bi-tramp
   (fib-cps 20 (empty-k jumpout))
   (fib-cps 5 (make-fib-k jumpout))))

;; ;;; step 5: inverse staging
;; (define fib-cps
;;   (λ (n k)
;;     (λ ()
;;       (cond
;;         ((<= n 1)
;;          (apply-k k 1))
;;         (else
;;          (fib-cps (sub1 n) (make-sub1-k n k)))))))

;; (define apply-k
;;   (λ (k v)
;;     (λ ()
;;       (match k
;;         (`(empty-k ,jumpout^)
;;          (begin
;;            (printf
;;             "Empty-k should only be invoked once!~n")
;;            (jumpout^ v)))
;;         (`(make-sub2-k ,v^ ,k^)
;;          (apply-k k^ (+ v^ v)))
;;         (`(make-sub1-k ,n^ ,k^)
;;          (fib-cps (sub2 n^)
;;                   (make-sub2-k v k^)))
;;         (`(make-car-k ,insertL*-cdr^ ,k^)
;;          (apply-k k^ (cons v insertL*-cdr^)))
;;         (`(make-cdr-k ,q^ ,r^ ,ls^ ,k^)
;;          (let ((insertL*-cdr v))
;;            (cond
;;              ((pair? (car ls^))
;;               (insertL*-cps q^ r^ (car ls^)
;;                (make-car-k insertL*-cdr k^)))
;;              ((eqv? (car ls^) q^)
;;               (apply-k k^
;;                (cons r^ (cons (car ls^) insertL*-cdr))))
;;              (else
;;               (apply-k k^ (cons (car ls^) insertL*-cdr))))))
;;         (`(make-fib-k ,jumpout^)
;;          (insertL*-cps 'x v '(a x (b x y z (x u)) j l z)
;;           (empty-k jumpout^)))))))

;; (define make-sub2-k
;;   (λ (v^ k^)
;;     `(make-sub2-k ,v^ ,k^)))

;; (define make-sub1-k
;;   (λ (n^ k^)
;;     `(make-sub1-k ,n^ ,k^)))

;; (define insertL*-cps
;;   (λ (q r ls k)
;;     (λ ()
;;       (cond
;;         ((null? ls)
;;          (apply-k k '()))
;;         (else
;;          (insertL*-cps q r (cdr ls)
;;           (make-cdr-k q r ls k)))))))

;; (define make-car-k
;;   (λ (insertL*-cdr^ k^)
;;     `(make-car-k ,insertL*-cdr^ ,k^)))

;; (define make-cdr-k
;;   (λ (q^ r^ ls^ k^)
;;     `(make-cdr-k ,q^ ,r^ ,ls^ ,k^)))

;; (define make-fib-k
;;   (λ (jumpout^)
;;     `(make-fib-k ,jumpout^)))

;; (define tramp
;;   (λ (th)
;;     (tramp (th))))

;; (define empty-k
;;   (λ (jumpout^)
;;     `(empty-k ,jumpout^)))

;; (let/cc jumpout
;;   (tramp (fib-cps 5 (empty-k jumpout))))

;; (let/cc jumpout
;;   (tramp (fib-cps 5 (make-fib-k jumpout))))
