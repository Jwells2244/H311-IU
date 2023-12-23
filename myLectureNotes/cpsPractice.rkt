#lang racket

(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only
          (error 'empty-k "You can only invoke the empty continuation once")
          (begin (set! once-only #t) v))))))


