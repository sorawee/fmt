#lang racket/base

(provide program-format

         pretty-comment
         extract

         current-max-blank-lines
         current-width

         (all-from-out "common.rkt"))

(require racket/match
         racket/list
         racket/string
         pprint-compact
         pprint-compact/memoize
         "common.rkt"
         "read.rkt"
         "realign.rkt")

(struct toplevel (xs) #:transparent)

(define current-width (make-parameter 102))
(define current-max-blank-lines (make-parameter 1))

(define (extract xs extract-configs)
  (let loop ([xs xs] [extract-configs extract-configs] [fits '()] [unfits '()])
    (match extract-configs
      ['() (list (reverse fits) (filter-not nl? (reverse unfits)) xs)]
      [(cons extract-config extract-configs)
       (match xs
         ['() #f]
         [(cons x xs)
          (cond
            [(visible? x) (cond
                            [(and extract-config (commentable-inline-comment x))
                             (loop xs
                                   extract-configs
                                   (cons (strip-comment x) fits)
                                   (cons (line-comment (commentable-inline-comment x)) unfits))]
                            [else (loop xs extract-configs (cons x fits) unfits)])]
            [else
             (loop xs (cons extract-config extract-configs) fits (cons x unfits))])])])))

(define (pretty-comment comment d) (if comment (full (hs-append d (text comment))) d))

(define (pretty d hook)
  (define loop
    (memoize
     (λ (d)
       (match d
         [(nl n) (full (v-concat (make-list n empty-doc)))]
         [(atom comment content _) (pretty-comment comment (text content))]
         [(line-comment comment) (full (text comment))]
         [(node _ _ _ _ _ xs) (match (extract xs (list #f))
                                [#f (((hook #f) loop) d)]
                                [(list (list (atom _ content 'symbol)) _ _) (((hook content) loop) d)]
                                [_ (((hook #f) loop) d)])]
         [(wrapper comment tok content)
          (pretty-comment comment (h-append (text tok) (loop content)))]
         [(sexp-comment comment style tok xs)
          (pretty-comment comment
                          (match style
                            ['newline (v-append (text tok) (v-concat (map loop xs)))]
                            ['any
                             (define :x (loop (first xs)))
                             (alt (h-append (text tok) :x) (v-append (text tok) :x))]
                            ['disappeared (loop (first xs))]))]
         [(toplevel xs) (v-concat (map loop xs))]))))
  (loop d))

(define (program-format program-source
                        formatter
                        #:source [source #f]
                        #:width [width (current-width)]
                        #:max-blank-lines [max-blank-lines (current-max-blank-lines)])
  (define s
    (pretty-format
     #:width width
     (pretty (toplevel (realign (read-all program-source source max-blank-lines)))
             formatter)))

  (string-join (for/list ([line (in-list (string-split s "\n"))])
                 (string-trim line #:left? #f))
               "\n"))
