#lang racket/base

(provide pretty-doc
         pretty-comment
         extract
         match/extract

         pretty
         doc
         define-pretty
         pretty-node

         try-indent)

(require racket/match
         racket/list
         racket/stxparam
         syntax/parse/define
         pprint-compact
         pprint-compact/memoize
         "common.rkt"
         (for-syntax racket/base syntax/parse/lib/function-header))

(define (extract xs extract-configs)
  (let loop ([xs xs] [extract-configs extract-configs] [fits '()] [unfits '()])
    (match extract-configs
      ['() (list (reverse fits) (filter-not nl? (reverse unfits)) xs)]
      [(cons extract-config extract-configs)
       (match xs
         ['() #f]
         [(cons x xs)
          (cond
            [(visible? x)
             (cond
               [(and extract-config (commentable-inline-comment x))
                (loop xs
                      extract-configs
                      (cons (strip-comment x) fits)
                      (cons (line-comment (commentable-inline-comment x)) unfits))]
               [else (loop xs extract-configs (cons x fits) unfits)])]
            [else (loop xs (cons extract-config extract-configs) fits (cons x unfits))])])])))

(define (pretty-comment comment d)
  (if comment (full (hs-append d (text comment))) d))

(define (pretty-doc xs hook)
  (define loop
    (memoize (λ (d)
               (match d
                 [(nl n) (full (v-concat (make-list n empty-doc)))]
                 [(atom comment content _) (pretty-comment comment (text content))]
                 [(line-comment comment) (full (text comment))]
                 [(node _ _ _ _ _ xs)
                  (match (extract xs (list #f))
                    [#f ((hook #f) d)]
                    [(list (list (atom _ content 'symbol)) _ _) ((hook content) d)]
                    [_ ((hook #f) d)])]
                 [(wrapper comment tok content)
                  (pretty-comment comment (h-append (text tok) (loop content)))]
                 [(sexp-comment comment style tok xs)
                  (pretty-comment comment
                                  (match style
                                    ['newline (v-append (text tok) (v-concat (map loop xs)))]
                                    ['any
                                     (define :x (loop (first xs)))
                                     (alt (h-append (text tok) :x) (v-append (text tok) :x))]
                                    ['disappeared (loop (first xs))]))]))))
  (set-box! current-pretty loop)
  (begin0 (v-concat (map loop xs))
    (set-box! current-pretty #f)))

(define (pretty-node* n d #:node [the-node n] #:unfits [unfits '()] #:adjust [adjust '("(" ")")])
  (match-define (node comment opener closer prefix breakable-prefix _) the-node)
  (define doc
    (pretty-comment comment
                    (h-append (text (string-append (or prefix "") (if adjust (first adjust) opener)))
                              d
                              (text (if adjust (second adjust) closer)))))
  (define doc*
    (if breakable-prefix
        (alt (h-append (text breakable-prefix) doc) (v-append (text breakable-prefix) doc))
        doc))
  (match unfits
    ['() doc*]
    [_ (v-append (v-concat (map (unbox current-pretty) unfits)) doc*)]))

(define current-pretty (box #f))
(define-syntax-parameter pretty
  (λ (stx) (raise-syntax-error #f "use of pretty outside its context" stx)))
(define-syntax-parameter doc (λ (stx) (raise-syntax-error #f "use of doc outside its context" stx)))

(begin-for-syntax
  (define-syntax-class header
    (pattern name:id)
    (pattern h:function-header
      #:with name #'h.name)))

(define-syntax-parse-rule (define-pretty head:header
                            #:type p?
                            {~seq #:default [from:id to]} ...
                            {~seq #:let [a:id b]} ...
                            body ...+)
  #:with ooo (quote-syntax ...)
  (define (head d)
    (let ([pretty-proc (unbox current-pretty)])
      (cond
        [(p? d)
         (syntax-parameterize ([pretty (make-rename-transformer #'pretty-proc)]
                               [doc (make-rename-transformer #'d)])
           (let* ([from (or from to)] ... [a b] ...)
             body ...))]
        [else (raise-argument-error 'head.name (symbol->string 'p?) d)]))))

(define-syntax-parse-rule (pretty-node args ...)
  (pretty-node* doc args ...))

(define (require-newline? d)
  (or (and (commentable? d) (commentable-inline-comment d)) (line-comment? d) (nl? d)))

(define (try-indent d #:n [n 1] #:because-of xs)
  (match xs
    ['() d]
    [_ (if (require-newline? (last xs)) (indent-next n d) d)]))

(define-syntax-parser match/extract
  [(_ xs #:as unfits tail [([pat req-status:boolean] ...) body ...+] . rst)
   #'(let ([-xs xs])
       (match (extract -xs '(req-status ...))
         [(list (list pat ...) unfits tail)
          body ...]
         [_
          (match/extract -xs #:as unfits tail
            . rst)]))]
  [(_ xs #:as unfits tail [#:else body ...+])
   #'(let ()
       body ...)])
