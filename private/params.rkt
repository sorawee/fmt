#lang racket/base

(provide :width
         :indent
         :selection
         :formatter
         :strip-trailing-spaces?
         :enabled?)

(define :width (make-parameter 102))
(define :indent (make-parameter 0))
(define :selection (make-parameter #f))
(define :formatter (make-parameter #f))
(define :strip-trailing-spaces? (make-parameter #t))
(define :enabled? (make-parameter #t))
