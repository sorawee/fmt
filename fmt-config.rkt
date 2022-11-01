#lang racket/base

(provide config-map)
(require fmt/config)

(define (config-map path)
  (config #:with :enabled? #f))
