#lang racket
(provide call/fresh run-g run-g! run*-g run*-g! disj conj conda once if-S-inf-te
         == fail succeed)

;;; A macro-free purely-functional-style version of
;;; microKanren. Vectors of length 1 are used to represent
;;; variables. This is The Curried microKanren.
;;; Refer to:
;;; https://link.springer.com/chapter/10.1007/978-3-031-38938-2_5
;;; ----------- logic variable -----------
;;; we improve our code by removing the global
;;; state, and pass a symbol to var through
;;; call/fresh as the variable's name
(define (var v)
  (vector v))

(define (var? x)
  (vector? x))
;;; ----------- substitution -----------
(define empty-S '())

;; When ext-s is invoked, x is a fresh variable, v can be
;; any term including a variable, but if it is the variable
;; x, it fails.
(define (ext-S x v S)
  (cond
    ((occurs? x v S) #f)
    (else (cons `(,x . ,v) S))))

;; x is fresh, v is any term, x cannot occur inside v or be
;; v.
(define (occurs? x v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) (eqv? v x))
      ((pair? v)
       (or (occurs? x (car v) s)
           (occurs? x (cdr v) s)))
      (else #f))))
;;; ----------- operations on substitutions -----------
;;; unify : (-> Any Any Substition (∪ Substitution #f))
; u, v are fresh variables or non-variables (e.g.: list,
; number); success of unification returns a substitution;
; failure returns false.

(define (unify u v S)
  (let ((u (walk u S)) (v (walk v S)))
    (cond
      ((eqv? u v) S)
      ((or (and (var? v) (ext-S v u S))
           (and (var? u) (ext-S u v S))))
      ((and (pair? u) (pair? v))
       ;; The name s^ is for clarity.
       (let ((s^ (unify (car u) (car v) S)))
         (and s^
              (unify (cdr u) (cdr v) s^))))
      (else #f))))

;;; walk : (-> Var Substition (∪ Any Var))
(define (walk v S)
  (let ((a (and (var? v) (assv v S))))
    (cond
      ((pair? a) (walk (cdr a) S))
      (else v))))

;;; ----------- Stream -----------
;;; Stream can be one of:
;;; - '()
;;; - (Substitution . Stream)
;;; - (λ () Stream)
(define (thunk? p)
  (zero? (procedure-arity p)))

;;; ----------- Goal -----------
;;; Goal : Substitution -> Stream
(define (== u v)
  (lambda (S)
    (let ((S^ (unify u v S)))
      (if S^ (list S^) (list)))))

(define succeed (== #f #f))

(define fail (== #f #t))

;;; ----------- Ways to introduce new goals -----------
;;; Way 1: introduce a new variable
;;; because var takes a symbol, now we pass the symbol, the
;;; name of a variable to call/fresh
;;; call/fresh : (-> Str (-> Var Goal) Goal)
(define (call/fresh y f)
  (f (var y)))


;;; Way 2: disjunction of a list of goals
(define ((disj . gs) S)
  (cond
    ((null? gs) (fail S))
    (else (D ((car gs) S) (cdr gs) S))))

(define (D S-inf gs S)
  (cond
    ((null? gs) S-inf)
    (else
     (append-inf S-inf
       (D ((car gs) S) (cdr gs) S)))))

(define (append-inf S-inf T-inf)
  (cond
    ((null? S-inf) T-inf)
    ((pair? S-inf)
     (cons (car S-inf)
       (append-inf (cdr S-inf) T-inf)))
    ;; We could use else here to replace (thunk? S-inf) if
    ;; other languages don't provide procedure-arity.
    ((thunk? S-inf)
     (lambda ()
       (append-inf T-inf (S-inf))))))

;;; Way 3: conjunction of a list of goals
(define ((conj . gs) S)
  (cond
    ((null? gs) (succeed S))
    (else (C (cdr gs) ((car gs) S)))))

(define (C gs S-inf)
  (cond
    ((null? gs) S-inf)
    (else
     (C (cdr gs)
        (append-map-inf (car gs) S-inf)))))

(define (append-map-inf g S-inf)
  (cond
    ((null? S-inf) '())
    ((pair? S-inf)
     ;; g doesn't change.
     (append-inf (g (car S-inf))
                 (append-map-inf g (cdr S-inf))))
    (else
     (lambda ()
       (append-map-inf g (S-inf))))))

;;; Way 4: only the first pair of goals whose car succeeds
;;; can contribute values
(define ((conda . g*) S)
  (cond
    ((null? g*) '())
    (else (A (cdr g*) ((car g*) S) S))))

;; When we transition to A, we know we have at least one
;; goal.
(define (A g* S-inf S)
  (cond
    ((null? g*) S-inf)
    ((null? (cdr g*)) (append-map-inf (car g*) S-inf))
    (else (if-S-inf-te S-inf (car g*) (cdr g*) S))))

(define (if-S-inf-te S-inf g g+ S)
  (cond
    ((null? S-inf) (A (cdr g+) ((car g+) S) S))
    ((pair? S-inf) (append-map-inf g S-inf))
    (else (λ () (if-S-inf-te (S-inf) g g+ S)))))

;;; Way 5: like conda, except that we consider only the
;;; first result of a successful question
(define ((once g) S)
  (O (g S)))

;;; O returns a Stream that can contain at most one
;;; Substitution.
(define (O S-inf)
  (cond
    ((null? S-inf) '())
    ;; We return a singleton substitution.
    ((pair? S-inf) (cons (car S-inf) '()))
    (else (λ () (O (S-inf))))))

;;; Way 6: project gives us the value associated with the
;;; var.
;;; project : (-> Var (-> Any Goal) Goal)
(define (project x f)
  (λ (S)
    (let ((x (walk* x S)))
      ((f x) S))))

;;; ----------- Ways to run a goal -----------
;;; run-g : (-> (∪ #f Int) (-> Var Goal) (Listof Any))
; Given a positive integer or #f, and a function that takes
; a Var and returns a goal, returns the results of unifying
; the goal; if given a positive integer, returns that many
; results if possible; if given #f, surprisingly returns all
; possible results.
(define (run-g n f)
  (call/fresh 'q
    (λ (v)
      (run-goal n (f v) v))))

;;; run*-g : (-> (-> Var Goal) (Listof Any))
(define (run*-g f)
  (run-g #f f))

(define (run-goal n g q)
  (match-let (((cons S-result S-rest)
               (reify-from-inf n (g empty-S) q '())))
    S-result))

(define (reify-from-inf n S-inf q S-result)
  (cond
    ((or (and n (zero? n)) (null? S-inf))
     (cons (reverse S-result) S-inf))
    ((pair? S-inf)
     (reify-from-inf (and n (sub1 n)) (cdr S-inf) q
       (cons ((reify q) (car S-inf)) S-result)))
    (else (reify-from-inf n (S-inf) q S-result))))

; Takes a value which is either a variable or non variable
; and after walk* is done, every variable has been replaced
; by its walked* value.

; e.g.:
; v = (x (y h) z (j 5 h))
; s = ((x . 2) (y . 3) (z . 4) (j . k))
; ((reify v) s) = (2 (3 h) 4 (k 5 h))
(define (reify v)
  (lambda (S)
    (let ((v (walk* v S)))
      (let ((S-env (reify-S v empty-S)))
        (walk* v S-env)))))

(define (walk* v S)
  (let ((v (walk v S)))
    (cond
      ((var? v) v)
      ((pair? v)
       (cons
         (walk* (car v) S)
         (walk* (cdr v) S)))
      (else v))))

; (2 (3 h) 4 (k 5 h))
; (2 (3 _0) 4 (_1 5 _0))
(define (reify-S v S-env)
  (let ((v (walk v S-env)))
    (cond
      ((var? v)
       (let ((n (length S-env)))
         (let ((rn (reify-name n)))
           (cons `(,v . ,rn) S-env))))
      ((pair? v)
       (let ((S-env^ (reify-S (car v) S-env)))
         (reify-S (cdr v) S-env^)))
      (else S-env))))

(define (reify-name n)
  (string->symbol
    (string-append "_" (number->string n))))

#;
(define-syntax defrel
  (syntax-rules ()
    ((defrel (name x ...) g ...)
     (define (name x ...)
       (lambda (S)
         (lambda ()
           ((conj g ...) S)))))))

#;
(defrel (appendo l1 s2 o)
  (disj
    (conj (== '() l1) (== s2 o))
    (call/fresh 'a
      (λ (a)
        (call/fresh 'b
          (λ (d)
            (call/fresh 'res
              (λ (res)
                (conj
                  (== `(,a . ,d) l1)
                  (== `(,a . ,res) o)
                  (appendo d s2 res))))))))))

(define (run-g! n f)
  (call/fresh 'q
    (λ (v)
      (run-goal! n (f v) v))))

(define (run*-g! f)
  (run-g! #f f))

(define (run-goal! n g q)
  (let ((S-inf (g empty-S)))
    (let ((next! (reify-from-stream! S-inf q)))
      (begin (next! n) next!))))

(define (reify-from-stream! S-inf q)
  (let ((S-inf^ S-inf))
    (λ (n)
      (match-let (((cons S-result S-inf-rest^)
                   (reify-from-inf n S-inf^ q '())))
        (set! S-inf^ S-inf-rest^)
        (format-result S-result n)))))

;;; example implementation of formatting the result
(define (format-result res query-n)
  (newline)
  (printf "Results requested:\n")
  (let ((res-len (length res)))
    (for ((i res-len)
          (r res))
      (printf "==========================\n")
      (printf "Result ")
      (when (not (= res-len 1))
        (printf "#~a" (add1 i)))
      (printf ": ~a \n" r))
    (when (and query-n (< res-len query-n))
      (printf "==========================\n")
      (printf "Only ~a result~a" res-len
              (if (> res-len 1) "s" ""))
      (when (and query-n (> query-n 0))
        (printf " out of ~a requested\n" query-n)))
    (newline)))
