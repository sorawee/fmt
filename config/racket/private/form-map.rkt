#lang racket/base

(provide form-map/c
         compose-form-map)

(require racket/contract)

(define form-map/c (-> (or/c string? #f) (or/c procedure? #f)))

(define ((compose-form-map . fs) x)
  (for/or ([f (in-list fs)])
    (f x)))

