#lang racket/base

(provide with-config
         default-config-map
         config)

(require racket/path
         syntax/parse/define
         "params.rkt"
         (prefix-in r: "../config/racket.rkt")
         (for-syntax racket/base))


(define-syntax-parse-rule (with-config c body ...+)
  (let ([c* c]
        [thk (λ () body ...)])
    (cond
      [c* (c* thk)]
      [else (thk)])))

(define-syntax-parse-rule (config x ...)
  (λ (thk) (config* thk x ...)))

(define-syntax-parser config*
  [(_ thk) #'(thk)]
  [(_ thk {~seq #:require [y ...]} xs ...)
   #'(let ()
       (local-require y ...)
       (config* thk xs ...))]
  [(_ thk {~seq #:with x:id v} xs ...)
   #'(parameterize ([x v])
       (config* thk xs ...))])

(define (default-config-map path)
  (cond
    [(path-has-extension? path ".rkt")
     (config #:with :formatter r:formatter)]
    [else (config #:with :enabled? #f)]))
