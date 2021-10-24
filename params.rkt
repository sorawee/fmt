#lang racket/base

(provide current-width
         current-max-blank-lines
         current-indent
         current-app?
         current-ellipsis?
         current-inline-limit)

(require racket/match
         racket/string
         "common.rkt")

(define current-width (make-parameter 102))
(define current-max-blank-lines (make-parameter 1))
(define current-indent (make-parameter 0))
(define current-app?
  (make-parameter
   (λ (d)
     (match d
       [(node _ (? app-prefix?) ")" _ _ _) #t]
       [_ #f]))))
(define current-inline-limit (make-parameter 32))
(define current-ellipsis?
  (make-parameter
   (λ (s) (regexp-match #px"^(\\.\\.\\.|\\.\\.\\.\\+|\\.\\.(\\d+))$" s))))

(define (app-prefix? s)
  (not (or (string-contains? s "#hash")
           (string-contains? s "#("))))
