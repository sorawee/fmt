#lang racket/base

(provide the-formatter-map)
(require fmt/conventions)

(define (the-formatter-map s)
  (case s
    [("match/extract") (format-uniform-body/helper 4 #:body-formatter (format-clause-2/indirect))]
    [("define-pretty") (standard-formatter-map "define")]
    [("define/record") (format-uniform-body/helper 3)]
    [else #f]))
