#lang racket/base

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
         pprint-compact
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
                        #:max-blank-lines [max-blank-lines (current-max-blank-lines)]
                        #:indent [indent (current-indent)])
  (define doc (realign (read-all program-source source max-blank-lines)))
  (define s
    (pretty-format
     #:width width
     #:indent indent
     (pretty-doc doc
                 (compose-formatter-map formatter-map standard-formatter-map))))

  (string-join (for/list ([line (in-list (string-split s "\n"))])
                 (string-trim line #:left? #f))
               "\n"))

(define ((compose-formatter-map . fs) x)
  (for/or ([f (in-list fs)])
    (f x)))

(define (empty-formatter-map _x) #f)

(define (pretty-format* x
                        #:width [width (current-width)]
                        #:formatter-map [formatter-map empty-formatter-map])
  (program-format (print x) #:formatter-map formatter-map #:width width))

(define (pretty-print* x
                       #:width [width (current-width)]
                       #:formatter-map [formatter-map empty-formatter-map])
  (display (pretty-format* x #:formatter-map formatter-map #:width width)))
