#lang racket

;; Today we will implement 4 interpreters which exibit
;; different parameter-passing conventions. But before that
;; we will introduce thunks.
;;Thunks are functions that take no arguments, and they allow us to delay the evaluation of something until
;explicitly called.
(define loop
  (λ ()
    (loop)))
;Just the racket if defined in a different way, except the conseq and alt will get evaluated if the condition is true or false respectively
(define ef ;And the test is also evaluated by those parenthesis
  (lambda (test conseq alt)
    (if (test)
        (conseq)
        (alt))))

;Evaluates to 11, because the 0 is evaluated to be zero, therefore triggering
;the conseq line, and that gets evaluated because it is in a thunk
(ef (lambda () (zero? 0))
    (lambda () (+ 3 8))
    (lambda () (* 3 8)))
;If we were to do this expression inside a normal if, with no thunks, we get an error, but if we remove the thunks and extra parenthesis
;it does evaluate to the right thing
;(if (lambda () (zero? 0))
;    (+ 3 8)
;    (* 3 8))
;This is a modified version of the ef that doesn't wrap the test condition to evaluate it, it assumes its not in a lambda
(define ef^
  (lambda (test conseq alt)
    (if test
        (conseq)
        (alt))))
;Both of these calls gives us 11, because the non '()' test case in ef^ makes it so we dont have to pass in a thunk for
; the (zero? 0) test condition for each of them. Having the empty lambda still works, but it is unneccessary.
(ef^ (zero? 0)
     (lambda () (+ 3 8))
     (lambda () (* 3 8)))

(ef^ (zero? 0)
     (lambda () (+ 3 8))
     (lambda () (loop)))
;Just standard interpreter
(define val-of
  (λ (e env)
    (match e
      (`,y #:when (symbol? y) (env y))
      (`,n #:when (number? n) n)
      (`(+ ,nexpr1 ,nexpr2)
       (+ (val-of nexpr1 env)
          (val-of nexpr2 env)))

      (`(λ (,x) ,body)
       #:when (symbol? x)
       (λ (arg)
         ; We also need to extend the environment
         (val-of body (λ (y)
                        (cond
                          ((eqv? y x) arg)
                          (else (env y)))))))
      (`(,rator ,rand)
       ((val-of rator env) (val-of rand env))))))

;; We will do so, by adding set! and begin to that language
;; that our interpreter is concerned with and using racket's boxes.
;; This works exactly like our old interpreter but,
;;  - enviroment now maps variables to boxed values instead of just values.
;This is now the call by value interpreter that is standard. Call by value syntax just means the arguments are evaluated and then placed
;into a function. This also uses boxes, but it is just the normal implementation where you unbox in the symbol case, and box the value-of-cbv rand env line in the rator rand
(define value-of-cbv
  (λ (e env)
    (match e
      (`,y #:when (symbol? y) (unbox (env y)))
      (`,n #:when (number? n) n)
      (`(+ ,nexpr1 ,nexpr2)
       (+ (value-of-cbv nexpr1 env)
          (value-of-cbv nexpr2 env)))
      (`(begin2 ,exp1 ,exp2)
       (begin (value-of-cbv exp1 env)
              (value-of-cbv exp2 env)))
      (`(set! ,x ,expr)
       #:when (symbol? x)
       (set-box! (env x)
                 (value-of-cbv expr env)))
      (`(λ (,x) ,body)
       #:when (symbol? x)
       (λ (arg)
         ; We also need to extend the environment
         (value-of-cbv body (λ (y)
                              (cond
                                ((eqv? y x) arg)
                                (else (env y)))))))
      (`(,rator ,rand)
       ((value-of-cbv rator env)
        (box (value-of-cbv rand env)))))))
;Empty environment, except we now know that this is a thunk
(define mt-env
  (λ ()
    (λ (y)
      (error 'empty-env "Unbound ~a~n" y))))

(printf "Reached the value-of-cbv tests ~n")
;This evaluates to 10 in standard call by value syntax because the arguments are first evaluated, and then placed into the function.
;This means the expression below really equals this:
;(value-of-cbv '(((λ (x)
;                   (λ (y)
;                     (begin2 (set! x y)
;                             (+ x y))))
;                 7) ;7 gets passed into the y lambda first, and in the y lambda expression, x is set to y, which makes it 7 as well, however the 5 getting passed into
;the x lambda means that it gets overwritten, so that y is set to 5 as well as x, so the addition makes them equal 10
;                5) ;5 gets passed into the x lambda
;              (mt-env))
(value-of-cbv '(((λ (x)
                   (λ (y)
                     (begin2 (set! x y)
                             (+ x y))))
                 7)
                5)
              (mt-env))

;This evaluates to 12 in standard cbv syntax, because it differs from the last expression with the addition of another lambda (x) in the begin
;This additional lambda (x) makes it so that when the y is passed into the lambda y, it can actually set x in there to be 7, and the x that is bounded
;by the first lambda gets the 5 input, and the addition then results in the 5 + 7
(value-of-cbv '(((λ (x)
                   (λ (y)
                     (begin2 ((λ (x)
                                (set! x y))
                              x)
                             (+ x y))))
                 7)
                5) ;5 gets passed into the x lambda, but that only replaces the second x in the expression, and the y still evaluates to 7
              (mt-env))
;This expression evaluates to 5, because similar to before, the set x to 3 would only matter if the 5 is not going to be passed
;into the final x lambda, which is evaluated last, and therefore placed into the function in the lambda y case, which equals that 5 when it is passed in
(value-of-cbv '((λ (x)
                  ((λ (y)
                     (begin2 (set! x 3)
                             y))
                   x))
                5)
              (mt-env))

;This expression evaluates to 5 as well, because the lambda (w) case at the top takes the 5 all the way down when the program reaches that point, into where it becomes z
;when z is returned, it is equivalent to that w|5
(value-of-cbv '((λ (w)
                 ((λ (x)
                    ((λ (y)
                      ((λ (z)
                         (begin2 (set! w 10)
                                 z))
                       w))
                     9))
                  7))
                5)
              (mt-env))

;; That was the first parameter passing convention,
;; Now for the second one, instead of passing values
;; we will be passing references to these values.
(printf "Reached the value-of-cbr tests except the first line ~n")
;; This returns 5 in usual cbv semantics.
;;We've been over this before, that x that gets passed in overwrites that whole set y 2 thing
(value-of-cbv '((λ (x)
                  ((λ (y)
                     (begin2 (set! y 2) x))
                   x))
              5)
            (mt-env))
;In value of cbr ^ this expression evaluates to 2
;; However, given the call-by-reference semantics, y will be
;; bound to the same reference in memory as the x was and so
;; when one is changed, the other will also change.
;;We have now added boxes to the interpreter to make it a call by reference interpreter
;;Call by reference means that arguments are evaluated and a reference to the result is passed into a function. This allows multiple variables
;to point to, reference, and alter the same piece of memory.
(define value-of-cbr
  (λ (e env)
    (match e
      (`,y #:when (symbol? y) (unbox (env y))) 
      (`,n #:when (number? n) n)
      (`(+ ,nexpr1 ,nexpr2)
       (+ (value-of-cbr nexpr1 env)
          (value-of-cbr nexpr2 env)))
      (`(begin2 ,exp1 ,exp2)
       (begin (value-of-cbr exp1 env)
              (value-of-cbr exp2 env)))
      (`(set! ,x ,expr)
       #:when (symbol? x)
       (set-box! (env x) ;Also implements side effects by adding the set! case with a set-box! call also in there. This is acknowledging
                 (value-of-cbr expr env))) ;that we have a box already, and are just changing the cdr of the box to (env x)
      (`(λ (,x) ,body)
       #:when (symbol? x)
       (λ (arg)
         ; We also need to extend the environment
         (value-of-cbr body (λ (y)
                              (cond
                                ((eqv? y x) arg)
                                (else (env y)))))))
      (`(,rator ,x)
       #:when (symbol? x) ;Biggest thing here is this new `rator case, where we evaluate the rator in the env and apply it to the env applied to x
       ((value-of-cbr rator env) (env x)))
      (`(,rator ,rand)
       ((value-of-cbr rator env)
        (box (value-of-cbr rand env)))))))
;;Adds a case to check for the rator as well as just x, and then adds a box to the rand call of value-of-cbr in the rator rand case.

;Now in cbr syntax, this expression evaluates to 3, because the x and y are both set to 3 in that innermost lambda,
;and this has made it so that when one changes, the other changes as well. If we put this back into the cbv interpreter we get 6, because that is the most
;recent use of the memory when in terms of x, but in this call by reference, the x and the y are both set in memory to 3, so the result is 3
(value-of-cbr '((λ (x)
                  ((λ (y)
                     (begin2 (set! x 3) y))
                   x))
                6)
              (mt-env))
;(value-of-cbv '((λ (x)
;                  ((λ (y)
;                     (begin2 (set! x 3) y))
;                   x))
;                6)
;              (mt-env))

;This expression in cbr evaluates to 10, because it is a similar situation, where the w and z variables are evaluated to the same place in memory
;which means that the w and z location in memory is set to 10, and therefore evaluated to 10.
(value-of-cbr '((λ (w)
                 ((λ (x)
                    ((λ (y)
                      ((λ (z)
                         (begin2 (set! w 10)
                                 z))
                       w))
                     9))
                  7))
                5)
              (mt-env))

;Simple fibonacci function
(define fib
  (λ (n)
    (cond
      ((<= n 1) 1)
      (else (+ (fib (sub1 n)) (fib (sub1 (sub1 n))))))))

;;Now we move to when stuff is evaluated
;; We now think of when is stuff evaluated?
;; The following ex. in both cbv and cbr would never halt
;; as those interpreters would need the value of operand
;; even if it is not needed and here since it is Ω we will
;; never get a value.
;This expression will never terminate, because it is using call by value syntax, because thats what racket
;uses. This means that it tries to evaluate the entire expression before assigning arguments, and tries to evaluate the loop
;To fix this, we can do what we normally do when enforcing evaluation only when it is needed.
;Thunks. So what we can do is to store what to evaluate and evaluate it as many times as needed.
;; ((λ (f)
;;    (if (zero? 0)
;;        5 f))
;;  ((loop)))

;; How do we enforce evaluation only when it is needed?
;; → Thunks aka λs with no argument.
;; So, what we can do is to store what to evaluate and
;; evaluate it as many times as needed.
(define value-of-cbna
  (λ (e env)
    (match e
      (`,y #:when (symbol? y)
       ;; Here we unbox the thunk and invoke it. This is similar to cbr, but this time we have an extra parenthesis around the unbox
       ((unbox (env y)))) ;to get the thunk, and the parenthesis allow us to invoke it.
      (`,n #:when (number? n) n)
      (`(+ ,nexpr1 ,nexpr2)
       (+ (value-of-cbna nexpr1 env)
          (value-of-cbna nexpr2 env)))
      (`(begin2 ,exp1 ,exp2)
       (begin (value-of-cbna exp1 env)
              (value-of-cbna exp2 env)))
      (`(set! ,x ,expr)
       #:when (symbol? x)
       (set-box! (env x)
                 (value-of-cbna expr env)))
      (`(fib ,n)
       #:when (number? n)
       (fib n))
      (`(loop) (loop))

      (`(λ (,x) ,body)
       #:when (symbol? x)
       (λ (arg)
         ; We also need to extend the environment
         (value-of-cbna body (λ (y)
                              (cond
                                ((eqv? y x) arg)
                                (else (env y)))))))
      (`(,rator ,x)
       #:when (symbol? x)
       ((value-of-cbna rator env) (env x)))

      (`(,rator ,rand)
       ((value-of-cbna rator env)
        ;; Here we bind a variable to a box of a thunk.
        (box (λ () ;Now in the rator rand case, instead of just assigning the value of cbna rand env, we bind it to a box of a thunk, in order to let it be evaluated later
               (printf "Expensive computation!!~n")
               (value-of-cbna rand env))))))))
;Infinite loop
(define Ω '((λ (x) (x x)) (λ (x) (x x))))
;Only gives us one expensive computation, because it evaluates the lambdas that are needed, we do not evaluate the omega infinite loop, because that is not neccessary,
(printf "CBNA Example 1~n") ;because that line is never invoked with a thunk.
(value-of-cbna `(((λ (x)
                   (λ (y)
                     x))
                 5000)
                ,Ω)
              (mt-env))
;This evaluates to 445, which is the result of passing fib 10 (89) into the lambda and basically times it by 5. This syntax however forces the evaluation of (fib 10)
;at each step of the lambda ladder, because it realizes that it has to be used over and over again, rather than evaluating it and placing it in the expression prior as in
;call by value
(printf "CBNA Example 2~n")
(value-of-cbna `((λ (n)
                   (+ n
                      (+ n
                         (+ n
                            (+ n n)))))
                 (fib 10))
               (mt-env))


;Call by need intepreter. Call by need means that the evaluation of arguments is put off until the argument is looked up. After the first evaluation, the
;result is stored back and used in all future requests.
(define value-of-cbne
  (λ (e env)
    (match e
      (`,y #:when (symbol? y)
       (let ((b (env y)))
         (let ((v ((unbox b))))
           (begin
             (set-box! b (λ () v))
             v))))  ;This changed symbol case is where the evaluation of arguments is put off. When it reaches this point, it has been looked up. It evaluates,
      (`,n #:when (number? n) n) ;and stores the result in a box b bound to a thunk so we can use it in future requests if needed.
      (`(+ ,nexpr1 ,nexpr2)
       (+ (value-of-cbne nexpr1 env)
          (value-of-cbne nexpr2 env)))
      (`(begin2 ,exp1 ,exp2)
       (begin (value-of-cbne exp1 env)
              (value-of-cbne exp2 env)))
      (`(set! ,x ,expr)
       #:when (symbol? x)
       (set-box! (env x)
                 (value-of-cbne expr env)))
      (`(fib ,n)
       #:when (number? n)
       (fib n))
      (`(loop) (loop))
      (`(λ (,x) ,body)
       #:when (symbol? x)
       (λ (arg)
         ; We also need to extend the environment
         (value-of-cbne body (λ (y)
                              (cond
                                ((eqv? y x) arg)
                                (else (env y)))))))
      (`(,rator ,x)
       #:when (symbol? x)
       ((value-of-cbne rator env) (env x)))

      (`(,rator ,rand)
       ((value-of-cbne rator env)
        (box (λ ()
               (printf "Expensive computation!!~n")
               (value-of-cbne rand env)))))))) ;Keeps the boxed thunk bound to the variable 

;This expression once again returns 5000, with only 1 expensive computation, and does not run the infinite loop because it is never
;being invoked to evaluate by a thunk.
(printf "CBNE Example 1~n")
(value-of-cbne `(((λ (x)
                   (λ (y)
                     x))
                 5000)
                ,Ω)
              (mt-env))

;Now we can see the difference, where this returns 25000, which is what we expect, but it only does 1 expensive computation again. That is because this
;5000 vaue is evaluated once, and then stored in, ready to be accessed again, instead of continually evaluating it each time it is referred to.
;This is true when we substitute the (fib 10) call in like the cbna call, it only evaluated that (fib 10) once to be 89, then kept referencing that evaluation
;when it was passed down.

(printf "CBNE Example 2~n")
(value-of-cbne `((λ (n)
                   (+ n
                      (+ n
                         (+ n
                            (+ n n)))))
                 5000)
               (mt-env))

;All 3 of these return infinite loops, because the (loop) is being called as one of the values attached to the lambda, which then tries to evaluate the (loop)
; and of course runs indefinitely.
;(printf "CBNA Example 3~n")
;(time
; (value-of-cbna '(((λ (x)
;                     (λ (y) (+ x x)))
;                   (fib 40))
;                  (loop))
;                (mt-env)))


;(printf "CBNA Example 3~n")
;(time
; (value-of-cbna '(((λ (x)
;                     (λ (y) (+ x x)))
;                   (fib 40))
;                  (loop))
;                (mt-env)))

;(printf "CBNE Example 3~n")
;(time
; (value-of-cbne '(((λ (x)
;                     (λ (y) (+ x x)))
;                   (fib 40))
;                  (loop))
;                (mt-env)))
