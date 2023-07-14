;; The main file for fmt

#lang racket/base

(define-logger fmt)

(provide program-format
         empty-formatter-map
         compose-formatter-map
         pretty-print*
         pretty-format*
         formatter-map/c
         (all-from-out "core.rkt")
         (all-from-out "params.rkt")
         (all-from-out "common.rkt")
         (all-from-out "conventions.rkt"))

(require racket/string
         racket/contract
         racket/format
         racket/match
         (except-in pretty-expressive flatten)
         "common.rkt"
         "core.rkt"
         "read.rkt"
         "realign.rkt"
         "params.rkt"
         "conventions.rkt")

(define formatter-map/c (-> (or/c string? #f) (or/c procedure? #f)))

(define (program-format program-source
                        #:formatter-map [formatter-map empty-formatter-map]
                        #:source [source #f]
                        #:width [width (current-width)]
                        #:limit [limit (current-limit)]
                        #:max-blank-lines [max-blank-lines (current-max-blank-lines)]
                        #:indent [indent (current-indent)])
  (define doc (realign (read-all program-source source max-blank-lines)))
  (match-define-values [(list (info s tainted? _)) _ real _]
    (time-apply
     (λ ()
       (pretty-format/factory/info
        (pretty-doc doc (compose-formatter-map formatter-map standard-formatter-map))
        (cost-factory
         (match-lambda**
          [((list b1 h1 c1) (list b2 h2 c2))
           (cond
             [(= b1 b2)
              (cond
                [(= h1 h2) (<= c1 c2)]
                [else (< h1 h2)])]
             [else (< b1 b2)])])
         (match-lambda**
          [((list b1 h1 c1) (list b2 h2 c2))
           (list (+ b1 b2) (+ h1 h2) (+ c1 c2))])
         (λ (c l)
           (define stop (+ c l))
           (cond
             [(> stop width)
              (define start (max width c))
              (define a (- start width))
              (define b (- stop start))
              (list (* b (+ (* 2 a) b)) 0 0)]
             [else (list 0 0 0)]))
         (λ (i) (list 0 1 0))
         limit)
        #:offset indent))
     '()))

  (define all-lines (string-split s "\n"))

  (log-message
   fmt-logger
   'debug
   'fmt
   (format "([duration ~a] [lines ~a] [tainted? ~a])"
           (exact->inexact (/ real 1000))
           (length all-lines)
           (if tainted? "true" "false"))
   #f
   #f)

  (string-join (for/list ([line (in-list all-lines)])
                 (string-trim line #:left? #f))
               "\n"))

(define ((compose-formatter-map . fs) x)
  (for/or ([f (in-list fs)])
    (f x)))

(define (empty-formatter-map _x) #f)

(define (pretty-format* x
                        #:width [width (current-width)]
                        #:formatter-map [formatter-map empty-formatter-map])
  (program-format (~s x) #:formatter-map formatter-map #:width width))

(define (pretty-print* x
                       #:width [width (current-width)]
                       #:formatter-map [formatter-map empty-formatter-map])
  (display (pretty-format* x #:formatter-map formatter-map #:width width)))
