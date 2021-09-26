#lang racket/base

(provide pretty-print*
         pretty-format*
         (all-from-out "core.rkt")
         (all-from-out "conventions.rkt"))

(require racket/cmdline
         racket/pretty
         "core.rkt"
         "conventions.rkt")

(define (pretty-format* x
                        #:width [width 80]
                        #:format [format standard-format])
  (program-format (pretty-format x) format #:width width))

(define (pretty-print* x
                       #:width [width 80]
                       #:format [format standard-format])
  (display (pretty-format* x #:width width #:format format)))
