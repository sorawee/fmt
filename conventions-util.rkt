#lang racket/base

(provide current-ellipsis?
         current-app?

         require-newline?
         pretty-v-concat/kw
         pretty-node
         try-indent

         extract*
         match/extract)

(require racket/match
         racket/list
         racket/string
         syntax/parse/define
         pprint-compact
         "core.rkt"
         (for-syntax racket/base))

(define current-ellipsis?
  (make-parameter
   (λ (s) (regexp-match #px"^(\\.\\.\\.|\\.\\.\\.\\+|\\.\\.(\\d+))$" s))))

(define (app-prefix? s)
  (not (or (string-contains? s "#hash")
           (string-contains? s "#("))))

(define current-app?
  (make-parameter
   (λ (d)
     (match d
       [(node _ (? app-prefix?) ")" _ _ _) #t]
       [_ #f]))))

(define ((pretty-v-concat/kw pretty) xs)
  (let loop ([xs xs])
    (match xs
      ['() empty-doc]
      [(list x) (pretty x)]
      [(list x (and ellipsis (atom _ (? (current-ellipsis?)) 'symbol)))
       #:when (not (require-newline? x))
       (alt
        (hs-append (pretty x) (pretty ellipsis))
        (v-append
         (pretty x)
         (pretty ellipsis)))]
      [(list x (and ellipsis (atom _ (? (current-ellipsis?)) 'symbol)) xs ...)
       #:when (not (require-newline? x))
       (alt
        (v-append
         (hs-append (pretty x) (pretty ellipsis))
         (loop xs))
        (v-append
         (pretty x)
         (pretty ellipsis)
         (loop xs)))]
      [(list (and kw (atom _ _ 'hash-colon-keyword))
             (and (? visible? v) (not (atom _ _ 'hash-colon-keyword))))
       #:when (not (require-newline? kw))
       (alt
        (hs-append (pretty kw) (pretty v))
        (v-append
         (pretty kw)
         (pretty v)))]
      [(list (and kw (atom _ _ 'hash-colon-keyword))
             (and (? visible? v) (not (atom _ _ 'hash-colon-keyword)))
             xs ...)
       #:when (not (require-newline? kw))
       (alt
        (v-append
         (hs-append (pretty kw) (pretty v))
         (loop xs))
        (v-append
         (pretty kw)
         (pretty v)
         (loop xs)))]
      [(list x xs ...) (v-append (pretty x) (loop xs))])))

(define (require-newline? d)
  (or (and (commentable? d) (commentable-inline-comment d))
      (line-comment? d)
      (nl? d)))

(define (pretty-node n d #:unfits [unfits '()] #:adjust [adjust #f])
  (match-define (node comment opener closer prefix breakable-prefix _) n)
  (define main-doc
    (pretty-comment
     comment
     (h-append (text (string-append (or prefix "")
                                    (if adjust (first adjust) opener)))
               d
               (text (if adjust (second adjust) closer)))))
  (define main-doc*
    (if breakable-prefix
        (alt (h-append (text breakable-prefix) main-doc)
             (v-append (text breakable-prefix) main-doc))
        main-doc))
  (match unfits
    ['() main-doc*]
    [_ (v-append (v-concat unfits) main-doc*)]))

(define (extract* pretty xs config)
  (match (extract xs config)
    [#f #f]
    [(list fits unfits tail)
     (list fits (map pretty unfits) tail)]))

(define (try-indent d #:n [n 1] #:because-of xs)
  (match xs
    ['() d]
    [_ (if (require-newline? (last xs))
           (indent-next n d)
           d)]))

(define-syntax-parser match/extract
  [(_ pretty xs #:as unfits tail
      [([pat req-status:boolean] ...) body ...+] . rst)
   #'(let ([-pretty pretty] [-xs xs])
       (match (extract* -pretty -xs '(req-status ...))
         [(list (list pat ...) unfits tail) body ...]
         [_ (match/extract -pretty -xs #:as unfits tail . rst)]))]
  [(_ pretty xs #:as unfits tail
      [#:else body ...+])
   #'(let () body ...)])
