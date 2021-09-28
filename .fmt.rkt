#lang racket/base

(provide the-formatter-map)
(require fmt/conventions)

(define (the-formatter-map s)
  (case s
    [("match/extract")
     (format-node-uniform-body 5 #:hook-for-body format-clause-2)]
    [else (standard-formatter-map s)]))
