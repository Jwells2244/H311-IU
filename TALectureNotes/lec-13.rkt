#lang racket
(require racket/trace)
;;Registerization lecture, Jonathan annotations.
;; (define insertL*
;;   (λ (q r ls)
;;     (cond
;;       ((null? ls) null)
;;       (else
;;        (let ((insert-L*-cdr (insertL* q r (cdr ls))))
;;          (cond
;;            ((pair? (car ls))
;;             (cons (insertL* q r (car ls))
;;                   insert-L*-cdr))
;;            ((eqv? (car ls) q)
;;             (cons r (cons q
;;                           insert-L*-cdr)))
;;            (else
;;             (cons (car ls)
;;                   insert-L*-cdr))))))))

;; (define insertL*-cps
;;   (λ (q r ls k)
;;     (cond
;;       ((null? ls) (k null))
;;       (else
;;        (insertL*-cps q r (cdr ls)
;;                      (λ (v)
;;                        (cond
;;                          ((pair? (car ls))
;;                           (insertL*-cps q r (car ls)
;;                                         (λ (w)
;;                                           (k (cons w v)))))
;;                          ((eqv? (car ls) q)
;;                           (k (cons r (cons q v))))
;;                          (else
;;                           (k (cons (car ls) v))))))))))


;; (define insertL*-cps
;;   (λ (q r ls k)
;;     (cond
;;       ((null? ls) (apply-k k null))
;;       (else
;;        (insertL*-cps q r (cdr ls)
;;                      (make-cdr-k q r ls k))))))

;; (define apply-k
;;   (λ (k v)
;;     (match k
;;       (`(empty-k) v)
;;       (`(make-car-k ,v^ ,k) (apply-k k (cons v v^)))
;;       (`(make-cdr-k ,q ,r ,ls ,k)
;;        (cond
;;          ((pair? (car ls))
;;           (insertL*-cps q r (car ls)
;;                         (make-car-k v k)))
;;          ((eqv? (car ls) q)
;;           (apply-k k (cons r (cons q v))))
;;          (else
;;           (apply-k k (cons (car ls) v))))))))

;; (define empty-k
;;   (λ ()
;;     `(empty-k)))

;; (define make-cdr-k
;;   (λ (q r ls k)
;;     `(make-cdr-k ,q ,r ,ls ,k)))

;; (define make-car-k
;;   (λ (v k)
;;     `(make-car-k ,v ,k)))

;This also evaluates to 19, because the i dont even know why man, something 
 (let* ((x 3)
       (y (+ x x))
       (x (+ y 7)))
   (+ x y))
;;In this, Ive figured it out now, y is set to be 6, because it is set when x is equal to 3, + x x, 6. Then, x is set to + 6 7
;Then + x and y is called, equalling 13 + 6.
;This expression does not work with just standard let, because let makes it so you can only set a variable once,
;you cant set the same variable at the same time.
;let performs binding parallel, while let* performs them sequentially.
;
;;This equals 19, because y is set when x is equal to 3, meaning y is set to be 6,
;Then, x is set to be 6 + 7, 13, then 13 + 6 = 19. This is because of let without the star does not
;dynamically scope things, it lexically scopes them where just because the x changes its value further down the lambda, the y
;does not change its value above it in scope.
(let ((x 3))
   (let ((y (+ x x)))
     (let ((x (+ y 7)))
       (+ x y))))

;; (define insertL*-cps
;;   (λ (*q* *r* *ls* *k*)
;;     (cond
;;       ((null? *ls*)
;;        (let* ((*a-k* *k*)
;;               (*v* null))
;;          (apply-k *a-k* *v*)))
;;       (else
;;        (let* ((*k* (make-cdr-k *q* *r* *ls* *k*))
;;               (*q* *q*)
;;               (*r* *r*)
;;               (*ls* (cdr *ls*)))
;;          (insertL*-cps *q* *r* *ls* *k*))))))

;; (define apply-k
;;   (λ (*a-k* *v*)
;;     (match *a-k*
;;       (`(empty-k) *v*)
;;       (`(make-car-k ,v^ ,k)
;;        (let* ((*a-k* k)
;;               (*v* (cons *v* v^)))
;;          (apply-k *a-k* *v*)))
;;       (`(make-cdr-k ,q^ ,r^ ,ls^ ,k^)
;;        (cond
;;          ((pair? (car ls^))
;;           (let* ((*k* (make-car-k *v* k^))
;;                  (*q* q^)
;;                  (*r* r^)
;;                  (*ls* (car ls^)))
;;             (insertL*-cps *q* *r* *ls* *k*)))
;;          ((eqv? (car ls^) q^)
;;           (let* ((*a-k* k^)
;;                  (*v* (cons r^ (cons q^ *v*))))
;;             (apply-k *a-k* *v*)))
;;          (else
;;           (let* ((*a-k* k^)
;;                  (*v* (cons (car ls^) *v*)))
;;             (apply-k *a-k* *v*))))))))

;; (define empty-k
;;   (λ ()
;;     `(empty-k)))

;; (define make-cdr-k
;;   (λ (q r ls k)
;;     `(make-cdr-k ,q ,r ,ls ,k)))

;; (define make-car-k
;;   (λ (v k)
;;     `(make-car-k ,v ,k)))
;You want to use let* here because some values may use other variables, and you want the call to be after all of these I think idk
;; (let* ((*k* (empty-k))
;;        (*q* 'x)
;;        (*r* 'y)
;;        (*ls* '(a x (b x y z (x u)) j l z)))
;;   (insertL*-cps *q* *r* *ls* *k*))
;The defining of the registers
;; (define *q* #f)
;; (define *v* #f)
;; (define *r* #f)
;; (define *ls* #f)
;; (define *k* #f)
;; (define *a-k* #f)

;; (define insertL*-cps
;;   (λ ()
;;     (cond
;;       ((null? *ls*)
;;        (begin (set! *a-k* *k*)
;;               (set! *v* null)
;;               (apply-k)))
;;       (else
;;        (begin (set! *k* (make-cdr-k *q* *r* *ls* *k*))
;;               (set! *q* *q*)
;;               (set! *r* *r*)
;;               (set! *ls* (cdr *ls*))
;;               (insertL*-cps))))))

;; (define apply-k
;;   (λ ()
;;     (match *a-k*
;;       (`(empty-k) *v*)
;;       (`(make-car-k ,v^ ,k)
;;        (begin (set! *a-k* k)
;;               (set! *v* (cons *v* v^))
;;               (apply-k)))
;;       (`(make-cdr-k ,q^ ,r^ ,ls^ ,k^)
;;        (cond
;;          ((pair? (car ls^))
;;           (begin (set! *k* (make-car-k *v* k^))
;;                  (set! *q* q^)
;;                  (set! *r* r^)
;;                  (set! *ls* (car ls^))
;;                  (insertL*-cps)))
;;          ((eqv? (car ls^) q^)
;;           (begin (set! *a-k* k^)
;;                  (set! *v* (cons r^ (cons q^ *v*)))
;;                  (apply-k)))
;;          (else
;;           (begin (set! *a-k* k^)
;;                  (set! *v* (cons (car ls^) *v*))
;;                  (apply-k))))))))

;; (define empty-k
;;   (λ ()
;;     `(empty-k)))

;; (define make-cdr-k
;;   (λ (q r ls k)
;;     `(make-cdr-k ,q ,r ,ls ,k)))

;; (define make-car-k
;;   (λ (v k)
;;     `(make-car-k ,v ,k)))

;We use set! because it mutates an existing binding.
;; (trace insertL*-cps)
;; (begin (set! *k* (empty-k))
;;        (set! *q* 'x)
;;        (set! *r* 'y)
;;        (set! *ls* '(a x (b x y z (x u)) j l z))
;;   (insertL*-cps))



(define insertL*-cps
  (λ (q r ls k)
    (cond
      ((null? ls) (apply-k k null))
      (else
       (insertL*-cps q r (cdr ls)
        (make-cdr-k q r ls k))))))

(define apply-k
  (λ (k v)
    (match k
      (`(empty-k) v)
      (`(make-car-k ,insertL*-cdr^ ,k^)
       (apply-k k^ (cons v insertL*-cdr^)))
      (`(make-cdr-k ,q^ ,r^ ,ls^ ,k^)
       (let ((insertL*-cdr v))
         (cond
           ((pair? (car ls^))
            (insertL*-cps q^ r^ (car ls^)
                          ;; start from innermost one
                          (make-car-k insertL*-cdr k^)))
           ((eqv? (car ls^) q^)
            (apply-k k^ (cons r^ (cons q^ insertL*-cdr))))
           (else
            (apply-k k^ (cons (car ls^) insertL*-cdr)))))))))

(define empty-k
  (λ ()
    `(empty-k)))

(define make-car-k
  (λ (insertL*-cdr^ k^)
    `(make-car-k ,insertL*-cdr^ ,k^)))

(define make-cdr-k
  (λ (q^ r^ ls^ k^)
    `(make-cdr-k ,q^ ,r^ ,ls^ ,k^)))

(insertL*-cps 'x 'y '(a x (b x y z (x u)) j l z)
              (empty-k))
