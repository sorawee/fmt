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
      [(cons (? nl? d) xs) (cons d (loop xs #f))]
      [(cons (? atom? d) xs) (cons d (loop xs #f))]
      [(cons (? line-comment? d) xs) (cons d (loop xs #f))]
      [(cons (? bare-sexp-comment?) xs)
       (define-values (invisibles tail) (splitf-at (loop (dropf xs nl?) #t) (negate visible?)))
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
               [(node comment opener closer prefix breakable-prefix content)
                (cons
                 (sexp-comment comment
                               'disappeared
                               ""
                               (list (struct-copy node
                                                  visible
                                                  [inline-comment #:parent commentable #f]
                                                  [breakable-prefix
                                                   (string-append "#;" (or breakable-prefix ""))])))
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
       (define-values (invisibles tail) (splitf-at (loop (dropf xs nl?) #f) (negate visible?)))
       (match tail
         ['() (raise-read-error "quote without content" #f #f #f #f #f)]
         [(cons visible xs)
          (append
           invisibles
           (cons
            (match visible
              ;; don't create a new wrapper, just transfer content
              [(wrapper _ tk* _) (struct-copy wrapper visible [tk (string-append tk tk*)])]
              [(node _ _ _ prefix breakable-prefix _)
               (case (string-length tk)
                 [(1) (struct-copy node visible [prefix (string-append tk (or prefix ""))])]
                 [else (struct-copy node
                                    visible
                                    [breakable-prefix (string-append tk (or breakable-prefix ""))])])]
              [_ (wrapper (commentable-inline-comment visible) tk (strip-comment visible))])
            xs))])]
      [(cons (node comment opener closer prefix breakable-prefix xs*) xs)
       (cons (node comment opener closer prefix breakable-prefix (loop xs* #f)) (loop xs #f))])))
