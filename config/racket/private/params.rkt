#lang racket/base

(provide app-prefix?

         :form-map
         :max-blank-lines
         :app?
         :ellipsis?)

(require racket/match
         racket/string
         "common.rkt")

(define :max-blank-lines (make-parameter 1))

(define :ellipsis?
  (make-parameter
   (λ (s)
     (regexp-match #px"^(\\.\\.\\.|\\.\\.\\.\\+|\\.\\.(\\d+))$"
                   s))))

(define :form-map (make-parameter #f))

(define :app?
  (make-parameter
   (λ (d)
     (match d
       [(node _ (? app-prefix?) ")" _ _ _) #t]
       [_ #f]))))

(define (app-prefix? s)
  (not (or (string-contains? s "#hash")
           (string-contains? s "#("))))
