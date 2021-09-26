#lang racket/base

(provide current-ellipsis?
         current-app?

         require-newline?
         pretty-v-concat/kw
         app-format/no-comment
         app-format/fail
         pretty-node

         extract*

         (struct-out app-format))

(require racket/match
         racket/list
         racket/string
         pprint-compact
         "core.rkt")

(struct app-format (d xs) #:transparent)

(define current-ellipsis?
  (make-parameter
   (λ (s) (regexp-match #px"^(\\.\\.\\.|\\.\\.\\.\\+|\\.\\.(\\d+))$" s))))

(define (app-prefix? s)
  (not (or (string-contains? s "#hash(")
           (string-contains? s "#("))))

(define current-app?
  (make-parameter
   (λ (d)
     (match d
       [(node _ (? app-prefix?) ")" _) #t]
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
  (or (thing-extra d)
      (line-comment? d)
      (nl? d)))

(define (app-format/no-comment d)
  (app-format d '()))

(define app-format/fail
  (app-format fail '()))

(define (pretty-node n xs #:adjust [adjust #f])
  (match-define (node comment opener closer _) n)
  (define (build-doc x)
    (match-define (app-format d xs) x)
    (let* ([main-doc (h-append (text (if adjust (first adjust) opener))
                               d
                               (text (if adjust (second adjust) closer)))]
           [main-doc (pretty-comment comment main-doc)])
      (match xs
        ['() main-doc]
        [_ (v-append (v-concat xs) main-doc)])))
  (apply alt (map build-doc xs)))

(define (extract* pretty xs config #:at-least [at-least (length config)])
  (match (extract xs config #:at-least at-least)
    [#f #f]
    [(list fits unfits tail)
     (list fits
           (λ (d) (app-format d (map pretty unfits)))
           tail)]))
