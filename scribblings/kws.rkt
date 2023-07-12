#lang racket/base

(provide get-kws)

(require racket/match
         pretty-expressive
         (only-in fmt/conventions all-kws))

(define (flow xs)
  (match xs
    ['() empty-doc]
    [(cons x xs)
     (let loop ([xs xs] [acc (text x)])
       (match xs
         ['() acc]
         [(list x xs ...)
          (define elem (text x))
          (loop xs
                (alt (v-append acc elem)
                     (as-append acc elem)))]))]))

(define (get-kws)
  (define xs (sort all-kws string<?))
  (pretty-format (flow xs) #:page-width 64))
