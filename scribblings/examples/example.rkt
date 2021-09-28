#lang racket

;; low :: number? -> listof number?
(define low
(λ (argument)
   (cond   (; this is the base case
        (zero? argument)
            '())
       (else
          (define
          content (* argument (+
          argument 1)
          (+ argument 2) (+ argument
          3)))
       (define
    next
    ( low ...........................
    ... ))    ; recursive call

(cons  content    next)))))



   ;; high :: number? -> listof number?
(  define high ...)
