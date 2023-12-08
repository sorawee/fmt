;; A tree fix up

#lang racket/base

(provide realign)
(require racket/match
         racket/function
         racket/list
         syntax/readerr
         "common.rkt"
         "read.rkt")

(define (realign xs)
  (let loop ([xs xs] [just-read-sexp-comment? #f])
    (match xs
      ['() '()]
      [(cons (? newl? d) xs) (cons d (loop xs #f))]
      [(cons (? atom? d) xs) (cons d (loop xs #f))]
      [(cons (? line-comment? d) xs) (cons d (loop xs #f))]
      [(cons (? bare-sexp-comment?) xs)
       (define-values (invisibles tail) (splitf-at (loop (dropf xs newl?) #t) (negate visible?)))
       (match tail
         ['() (raise-read-error "sexp-comment without content" #f #f #f #f #f)]
         [(cons visible xs)
          (match invisibles
            [(cons (sexp-comment comment style? tok content) invisibles)
             ;; style is NOT 'disappeared because if that's the case,
             ;; the current fragment wouldn't be bare-sexp-comment
             (append (list (sexp-comment comment style? (string-append "#;" tok) content))
                     invisibles
                     (cons visible xs))]
            ['()
             #:when (not just-read-sexp-comment?)
             (match visible
               [(node comment opener closer prefix content)
                (cons
                 (sexp-comment comment
                               'disappeared
                               ""
                               (list (struct-copy node
                                                  visible
                                                  [inline-comment #:parent commentable #f]
                                                  [prefix (cons (cons 'breakable "#;") prefix)])))
                 xs)]
               [_ (cons (sexp-comment (commentable-inline-comment visible)
                                      'any
                                      "#;"
                                      (list (strip-comment visible)))
                        xs)])]
            [_ (cons (sexp-comment (commentable-inline-comment visible)
                                   'newline
                                   "#;"
                                   (append invisibles (list (strip-comment visible))))
                     xs)])])]

      [(cons (bare-prefix tk) xs)
       (define-values (invisibles tail) (splitf-at (loop (dropf xs newl?) #f) (negate visible?)))
       (match tail
         ['() (raise-read-error "quote without content" #f #f #f #f #f)]
         [(cons visible xs)
          (append
           invisibles
           (cons
            (match visible
              ;; don't create a new wrapper, just transfer content
              [(wrapper _ tk* _) (struct-copy wrapper visible [tk (string-append tk tk*)])]
              [(node _ _ _ prefix _)
               (match tk
                 [(app string-length 1)
                  (struct-copy node visible [prefix (cons (cons 'unbreakable tk) prefix)])]
                 [_ (struct-copy node visible [prefix (cons (cons 'breakable tk) prefix)])])]
              [_ (wrapper (commentable-inline-comment visible) tk (strip-comment visible))])
            xs))])]
      [(cons (node comment opener closer prefix xs*) xs)
       (cons (node comment opener closer prefix (loop xs* #f)) (loop xs #f))])))
