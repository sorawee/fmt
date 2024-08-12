#lang racket/base

(provide define/memoize)

(require syntax/parse/define)

(define (memoize f #:backend [backend make-weak-hasheq])
  (define table (backend))
  (λ (x) (hash-ref! table x (λ () (f x)))))

(define-syntax-parse-rule (define/memoize (function:id arg:id) body:expr ...+)
  (define function (memoize (λ (arg) body ...))))
