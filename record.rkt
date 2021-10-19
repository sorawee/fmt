#lang racket/base

(provide define/record)
(require syntax/parse/define
         (for-syntax racket/base))

(define-syntax-parse-rule (define/record head #:record x:id
                            ({~literal case} e
                                             [(s ...) body ...] ...
                                             [{~literal else} else-body ...]))
  (begin
    (define x (list {~@ s ...} ...))
    (define head
      (case e
        [(s ...) body ...] ...
        [else else-body ...]))))
