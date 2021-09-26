#lang racket/base

(provide standard-lookup)
(require racket/match
         racket/list
         pprint-compact
         "core.rkt"
         "conventions-util.rkt")

(define ((format-#%app pretty) d)
  (define xs (node-content d))
  (cond
    [((current-app?) d)
     (alt
      (match (extract* pretty xs (list #f))
        [#f (pretty-node d (try-indent
                            #:n 0
                            #:because-of xs
                            ((pretty-v-concat/kw pretty) xs)))]
        [(list (list -head) unfits tail)
         (pretty-node
          #:unfits unfits
          d
          (try-indent
           #:n 0
           #:because-of (cons -head tail)
           ((pretty-v-concat/kw pretty) (cons -head tail))))])
      (match (extract* pretty xs (list #t))
        [#f fail]
        [(list (list -head) unfits tail)
         (pretty-node
          #:unfits unfits
          d
          (hs-append (flat (pretty -head))
                     (try-indent
                      #:n 0
                      #:because-of tail
                      (alt
                       ;; try to fit in one line
                       #;(a #:x a b c)
                       (flat (hs-concat (map pretty tail)))

                       #;(aaaaaaaaaaaaa #:x aaaaaaaaaaaaaaa
                                        bbbbbbbbbbbbbbb
                                        cccccccccccccccc)
                       ((pretty-v-concat/kw pretty) tail)))))]))]
    [else
     (pretty-node
      d
      (alt ((pretty-v-concat/kw pretty) xs)
           (flat (hs-concat (map pretty xs)))))]))

(define ((format-define-like pretty) d)
  (match (extract* pretty (node-content d) (list #t #f))
    ;; stuff like (define) -- don't care
    [#f ((format-#%app pretty) d)]
    ;; this is it!
    [(list (list -define -head) unfits tail)
     (pretty-node
      #:unfits unfits
      d
      (try-indent
       #:because-of tail
       (alt
        ;; fit in one line case; only when there are exactly three things
        #;(define a b)
        (match tail
          [(list -e)
           (flat (hs-concat (map pretty (list -define -head -e))))]
          [_ fail])

        ;; general case
        #;(define (a b)
            c
            d
            e)
        (v-append
         (hs-append (pretty -define) (pretty -head))
         (h-append space (v-concat (map pretty tail)))))))]))

(define ((format-clause-2 pretty) clause)
  (match clause
    [(node _ (or "(" "[") (or ")" "]") xs)
     (alt
      ;; try to fit in one line; only when there are exactly two things
      #;[a b]
      (match (extract* pretty xs (list #t #f))
        [(list (list -head -something) unfits '())
         (pretty-node
          #:adjust '("[" "]")
          #:unfits unfits
          clause
          (try-indent
           #:n 0
           #:because-of (list -something)
           (hs-append (flat (pretty -head)) (pretty -something))))]
        [_ fail])

      ;; general case
      (pretty-node
       #:adjust '("[" "]")
       clause
       (try-indent
        #:n 0
        #:because-of xs
        ((pretty-v-concat/kw pretty) xs))))]
    [_ (pretty clause)]))

(define (((format-with-uniform-body
           n
           #:hook-for-arg [hook-for-arg values]
           #:hook-for-body [hook-for-body values]
           #:require-body? [require-body? #t])
          pretty)
         d)
  (match (extract* pretty (node-content d) (append (make-list n #t) (list #f)))
    ;; don't care
    [#f ((format-#%app pretty) d)]
    ;; this is it!
    [(list (list -macro-name -e ...) unfits tail)
     (define first-line
       (match -e
         ['() (pretty -macro-name)]
         [_
          (define args (map (hook-for-arg pretty) -e))
          (hs-append (pretty -macro-name)
                     (alt (flat (hs-concat args)) (v-concat args)))]))
     (pretty-node
      #:unfits unfits
      d
      (try-indent
       #:because-of (append (cons -macro-name -e) tail)
       (match tail
         ['()
          #:when (not require-body?)
          first-line]
         [_ (v-append
             first-line
             (h-append
              space
              ((pretty-v-concat/kw (hook-for-body pretty)) tail)))])))]))

(define ((format-binding-pairs pretty) bindings)
  (match bindings
    [(node _ _ _ xs)
     (pretty-node
      bindings
      (try-indent
       #:n 0
       #:because-of xs
       (alt
        ;; try to fit in one line
        (flat (hs-concat (map pretty xs)))
        ;; general case
        (v-concat (map pretty xs)))))]
    [_ (pretty bindings)]))

(define ((format-let-like pretty) d)
  (match (extract* pretty (node-content d) (list #t #f))
    [#f ((format-#%app pretty) d)]
    ;; named let
    [(list (list _ (? atom?)) _ _)
     (=> fail-pattern)
     (match (extract* pretty (node-content d) (list #t #t #f))
       [#f (fail-pattern)]
       [(list (list -let -name -bindings) unfits tail)
        (pretty-node
         #:unfits unfits
         d
         (try-indent
          #:because-of tail
          (v-append
           (hs-append (pretty -let)
                      (pretty -name)
                      ((format-binding-pairs pretty) -bindings))
           (h-append space (v-concat (map pretty tail))))))])]
    ;; regular let
    [_ ((format-let*-like pretty) d)]))

(define ((format-let*-like pretty) d)
  (match (extract* pretty (node-content d) (list #t #f))
    [#f ((format-#%app pretty) d)]
    ;; regular let
    [(list (list -let -bindings) unfits tail)
     (pretty-node
      #:unfits unfits
      d
      (try-indent
       #:because-of tail
       (alt
        ;; try to fit in one line; only when there are exactly 3 things
        (match tail
          #;(let ([x y] [z x]) z)
          [(list -body)
           (flat (hs-append (pretty -let)
                            ((format-binding-pairs pretty) -bindings)
                            (pretty -body)))]
          [_ fail])

        ;; general case
        #;(let ([x y])
            x
            y)
        (v-append
         (hs-append (pretty -let) ((format-binding-pairs pretty) -bindings))
         (h-append
          space
          (v-concat (map pretty tail)))))))]))

(define ((format-require-like pretty) d)
  (match (extract* pretty (node-content d) (list #t))
    [(list (list -provide) unfits tail)
     (pretty-node
      #:unfits unfits
      d
      (match tail
        ['() (pretty -provide)]
        [_
         (hs-append (pretty -provide)
                    (try-indent
                     #:n 0
                     #:because-of tail
                     (v-concat (map pretty tail))))]))]))

(define ((format-struct-like pretty) d)
  (match (extract* pretty (node-content d) (list #t #t #f))
    [#f ((format-#%app pretty) d)]
    [(list (list _ _ (? atom?)) unfits tail)
     (((format-with-uniform-body 3 #:require-body? #f) pretty) d)]
    [_ (((format-with-uniform-body 2 #:require-body? #f) pretty) d)]))

(define (standard-lookup name)
  (case name
    ;; always in the form
    #;(provide a
               b
               c)
    [("provide" "require") format-require-like]
    ;; try to fit in one line is the body has exactly one form,
    ;; else will be multiple lines
    #;(define x 1)
    #;(define (y)
        a
        b)
    [("define" "define-for-syntax" "define-values") format-define-like]
    [("define-syntax" "define-syntaxes" "define-values-for-syntax")
     format-define-like]
    [("λ" "lambda") format-define-like]

    [("let*" "let-values" "let*-values" "letrec" "letrec-values")
     format-let*-like]
    [("let-syntax" "letrec-syntax" "let-syntaxes" "letrec-syntaxes")
     format-let*-like]
    [("with-syntax" "with-syntax*") format-let*-like]
    [("parameterize" "parameterize*") format-let*-like]
    [("letrec-syntaxes+values")
     (format-with-uniform-body 2 #:hook-for-arg format-binding-pairs)]

    [("begin") (format-with-uniform-body 0)]
    [("begin0") (format-with-uniform-body 1)]
    [("module+") (format-with-uniform-body 1)]
    [("module" "module*") (format-with-uniform-body 2)]

    [("cond" "case-lambda")
     (format-with-uniform-body 0 #:hook-for-body format-clause-2)]

    [("syntax-rules" "syntax-parse" "match" "match*" "case")
     (format-with-uniform-body 1 #:hook-for-body format-clause-2)]

    [("syntax-case")
     (format-with-uniform-body 2 #:hook-for-body format-clause-2)]

    [("syntax/loc" "quasisyntax/loc") (format-with-uniform-body 1)]
    [("when" "unless") (format-with-uniform-body 1)]

    ;; these are really hacks... they don't support kws in body like #:break well
    [("for/fold" "for*/fold")
     (format-with-uniform-body 2 #:hook-for-arg format-binding-pairs)]
    [("for" "for*")
     (format-with-uniform-body 1 #:hook-for-arg format-binding-pairs)]
    [("for/list" "for*/list")
     (format-with-uniform-body 1 #:hook-for-arg format-binding-pairs)]
    [("for/hash" "for*/hash")
     (format-with-uniform-body 1 #:hook-for-arg format-binding-pairs)]
    [("for/hasheq" "for*/hasheq")
     (format-with-uniform-body 1 #:hook-for-arg format-binding-pairs)]
    [("for/hasheqv" "for*/hasheqv")
     (format-with-uniform-body 1 #:hook-for-arg format-binding-pairs)]
    [("for/vector" "for*/vector")
     (format-with-uniform-body 1 #:hook-for-arg format-binding-pairs)]

    ;; support both named let and usual let
    [("let") format-let-like]

    [("struct") format-struct-like]
    [("define-struct") (format-with-uniform-body 2 #:require-body? #f)]

    [else format-#%app]))
