#lang racket/base

(provide standard-lookup)
(require racket/match
         racket/list
         pprint-compact
         "core.rkt"
         "conventions-util.rkt")

(define ((format-node-#%app pretty) d)
  (define xs (node-content d))
  (cond
    [((current-app?) d)
     (match/extract pretty xs #:as unfits tail
       [([-head #f])
        (alt
         ;; mostly vertical
         (pretty-node
          #:unfits unfits
          d
          (try-indent
           #:n 0
           #:because-of (cons -head tail)
           ((pretty-v-concat/kw pretty) (cons -head tail))))

         ;; pretty cases
         (match/extract pretty xs #:as unfits tail
           [([-head #t] [-first-arg #f])
            (pretty-node
             #:unfits unfits
             d
             (hs-append (flat (pretty -head))
                        (try-indent
                         #:n 0
                         #:because-of (cons -first-arg tail)
                         (alt
                          ;; try to fit in one line
                          #;(a #:x a b c)
                          (flat (hs-concat (map pretty (cons -first-arg tail))))

                          #;(aaaaaaaaaaaaa #:x aaaaaaaaaaaaaaa
                                           bbbbbbbbbbbbbbb
                                           cccccccccccccccc)
                          ((pretty-v-concat/kw pretty)
                           (cons -first-arg tail))))))]
           [#:else fail]))]
       [#:else
        ;; perhaps full of comments, or there's nothing at all
        (pretty-node d (try-indent
                        #:n 0
                        #:because-of xs
                        ((pretty-v-concat/kw pretty) xs)))])]
    [else
     (pretty-node
      d
      (try-indent
       #:n 0
       #:because-of xs
       (alt
        ;; general case
        ((pretty-v-concat/kw pretty) xs)
        ;; try to fit in one line
        (flat (hs-concat (map pretty xs))))))]))

(define (((format-node-define #:hook-for-head [hook-for-head values]) pretty) d)
  (match/extract pretty (node-content d) #:as unfits tail
    [([-define #t] [-head #f])
     (alt
      ;; fit in one line case; only when there are exactly three things
      #;(define a b)
      (pretty-node
       #:unfits unfits
       d
       (try-indent
        #:because-of tail
        (alt

         (match tail
           [(list -e)
            (flat (hs-append (pretty -define)
                             ((hook-for-head pretty) -head)
                             (pretty -e)))]
           [_ fail]))))

      ;; general case
      #;(define (a b)
          c
          d
          e)
      (((format-node-uniform-body
         1
         #:hook-for-arg hook-for-head)
        pretty)
       d))]
    [#:else ((format-node-#%app pretty) d)]))

(define ((format-clause-2 pretty) clause)
  (match clause
    [(node _ (or "(" "[") (or ")" "]") xs)
     (alt
      ;; try to fit in one line; only when there are exactly two things
      #;[a b]
      (match/extract pretty xs #:as unfits tail
        [([-head #t] [-something #f])
         (match tail
           ['() (pretty-node
                 #:adjust '("[" "]")
                 #:unfits unfits
                 clause
                 (try-indent
                  #:n 0
                  #:because-of (list -something)
                  (hs-append (flat (pretty -head)) (pretty -something))))]
           [_ fail])]
        [#:else fail])

      ;; general case
      (pretty-node
       #:adjust '("[" "]")
       clause
       (try-indent
        #:n 0
        #:because-of xs
        ((pretty-v-concat/kw pretty) xs))))]
    [_ (pretty clause)]))

(define (((format-node-uniform-body
           n
           #:hook-for-arg [hook-for-arg values]
           #:hook-for-body [hook-for-body values]
           #:require-body? [require-body? #t])
          pretty)
         d)
  (match (extract* pretty (node-content d) (append (make-list n #t) (list #f)))
    ;; don't care
    [#f ((format-node-#%app pretty) d)]
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
        (flat (hs-concat (map (format-clause-2 pretty) xs)))
        ;; general case
        (v-concat (map (format-clause-2 pretty) xs)))))]
    [_ (pretty bindings)]))

(define ((format-node-let pretty) d)
  (match/extract pretty (node-content d) #:as unfits tail
    ;; named let
    [([-let #t] [(? atom? -name) #t] [(? node? -bindings) #f])
     (pretty-node
      #:unfits unfits
      d
      (try-indent
       #:because-of tail
       (v-append
        (hs-append (pretty -let)
                   (pretty -name)
                   ((format-binding-pairs pretty) -bindings))
        (h-append space (v-concat (map pretty tail))))))]
    ;; regular let
    [#:else ((format-node-let* pretty) d)]))

(define format-node-let*
  (format-node-define #:hook-for-head format-binding-pairs))

(define ((format-node-require pretty) d)
  (match/extract pretty (node-content d) #:as unfits tail
    [([-provide #t])
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
                     (v-concat (map pretty tail))))]))]
    [#:else (error 'impossible)]))

(define ((format-node-struct pretty) d)
  (match/extract pretty (node-content d) #:as unfits tail
    [([_ #t] [_ #t] [(? atom?) #f])
     (((format-node-uniform-body 3 #:require-body? #f) pretty) d)]
    [#:else (((format-node-uniform-body 2 #:require-body? #f) pretty) d)]))

(define (standard-lookup name)
  (case name
    ;; always in the form
    #;(provide a
               b
               c)
    [("provide" "require") format-node-require]
    ;; try to fit in one line is the body has exactly one form,
    ;; else will be multiple lines
    #;(define x 1)
    #;(define (y)
        a
        b)
    [("define" "define-for-syntax" "define-values") (format-node-define)]
    [("define-syntax" "define-syntaxes" "define-values-for-syntax")
     (format-node-define)]
    [("λ" "lambda") (format-node-define)]

    [("let*" "let-values" "let*-values" "letrec" "letrec-values")
     format-node-let*]
    [("let-syntax" "letrec-syntax" "let-syntaxes" "letrec-syntaxes")
     format-node-let*]
    [("with-syntax" "with-syntax*") format-node-let*]
    [("parameterize" "parameterize*") format-node-let*]
    [("letrec-syntaxes+values")
     (format-node-uniform-body 2 #:hook-for-arg format-binding-pairs)]

    [("begin") (format-node-uniform-body 0)]
    [("begin0") (format-node-uniform-body 1)]
    [("module+") (format-node-uniform-body 1)]
    [("module" "module*") (format-node-uniform-body 2)]

    [("cond" "case-lambda")
     (format-node-uniform-body 0 #:hook-for-body format-clause-2)]

    [("syntax-rules" "syntax-parse" "match" "match*" "case")
     (format-node-uniform-body 1 #:hook-for-body format-clause-2)]

    [("syntax-case")
     (format-node-uniform-body 2 #:hook-for-body format-clause-2)]

    [("syntax/loc" "quasisyntax/loc") (format-node-uniform-body 1)]
    [("when" "unless") (format-node-uniform-body 1)]

    ;; these are really hacks... they don't support kws in body like #:break well
    [("for/fold" "for*/fold")
     (format-node-uniform-body 2 #:hook-for-arg format-binding-pairs)]
    [("for" "for*")
     (format-node-uniform-body 1 #:hook-for-arg format-binding-pairs)]
    [("for/list" "for*/list")
     (format-node-uniform-body 1 #:hook-for-arg format-binding-pairs)]
    [("for/hash" "for*/hash")
     (format-node-uniform-body 1 #:hook-for-arg format-binding-pairs)]
    [("for/hasheq" "for*/hasheq")
     (format-node-uniform-body 1 #:hook-for-arg format-binding-pairs)]
    [("for/hasheqv" "for*/hasheqv")
     (format-node-uniform-body 1 #:hook-for-arg format-binding-pairs)]
    [("for/vector" "for*/vector")
     (format-node-uniform-body 1 #:hook-for-arg format-binding-pairs)]

    ;; support both named let and usual let
    [("let") format-node-let]

    [("struct") format-node-struct]
    [("define-struct") (format-node-uniform-body 2 #:require-body? #f)]

    [else format-node-#%app]))
