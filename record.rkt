#lang racket/base

(provide define/record)
(require syntax/parse/define
         (for-syntax racket/base))

(define-syntax-parse-rule (define/record fun-name #:record x:id
                            [(s ...) body ...] ...
                            [{~literal else} else-body ...])
  (begin
    (define x (list {~@ s ...} ...))
    (define (fun-name x)
      (case x
        [(s ...) body ...] ...
        [else else-body ...]))))
