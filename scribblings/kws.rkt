#lang racket/base

(provide get-kws)

(require racket/match
         pprint-compact
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
                     (hs-append acc elem)))]))]))

(define (get-kws)
  (define xs (sort all-kws string<?))
  (pretty-format (flow xs) #:width 64))
