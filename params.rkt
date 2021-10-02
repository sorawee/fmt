#lang racket/base

(provide current-width
         current-max-blank-lines
         current-indent)

(define current-width (make-parameter 102))
(define current-max-blank-lines (make-parameter 1))
(define current-indent (make-parameter 0))
