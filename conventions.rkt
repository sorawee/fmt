#lang racket/base

(provide standard-formatter-map
         format-node-if-like

         format-node-#%app

         format-node-uniform-body

         format-clause-2
         format-binding-pairs

         format-node-define
         format-node-let
         format-node-let*
         format-node-require
         format-node-if
         format-node-struct)
(require racket/match
         racket/list
         pprint-compact
         "core.rkt"
         "conventions-util.rkt")

(define (((format-node-if-like hook-else) pretty) d)
  (define xs (node-content d))
  (match/extract pretty xs #:as unfits tail
    [([-if #t] [-conditional #f])
     (pretty-node
      #:unfits unfits
      d
      (hs-append
       (flat (pretty -if))
       (try-indent
        #:n 0
        #:because-of (cons -conditional tail)
        ;; try to fit in one line
        #;(if a b c)
        (alt (flat (hs-concat (map pretty (cons -conditional tail))))
             ;; or multiple lines
             #;(if aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
                   bbbbbbbbbbbbbbbbbbbbb
                   ccccccccccccccc)
             ((pretty-v-concat/kw pretty) (cons -conditional tail))))))]
    [#:else ((hook-else pretty) d)]))

(define ((format-node-#%app pretty) d)
  (define xs (node-content d))
  (cond
    [((current-app?) d)
     (match/extract pretty xs #:as unfits tail
       [([-head #f])
        ;; mostly vertical
        (alt (pretty-node #:unfits unfits
                          d
                          (try-indent #:n 0
                                      #:because-of (cons -head tail)
                                      ((pretty-v-concat/kw pretty)
                                       (cons -head tail))))

             ;; pretty cases
             (((format-node-if-like (λ (pretty) (λ (d) fail))) pretty) d))]
       ;; perhaps full of comments, or there's nothing at all
       [#:else (pretty-node d
                            (try-indent #:n 0
                                        #:because-of xs
                                        ((pretty-v-concat/kw pretty) xs)))])]
    [else (pretty-node d
                       (try-indent #:n 0
                                   #:because-of xs
                                   ;; general case
                                   (alt ((pretty-v-concat/kw pretty) xs)
                                        ;; try to fit in one line
                                        (flat (hs-concat (map pretty xs))))))]))

(define (((format-node-uniform-body n
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
     (pretty-node #:unfits unfits
                  d
                  (try-indent #:because-of (append (cons -macro-name -e) tail)
                              (match tail
                                ['()
                                 #:when (not require-body?)
                                 first-line]
                                [_ (v-append first-line
                                             (h-append space
                                                       ((pretty-v-concat/kw
                                                         (hook-for-body pretty))
                                                        tail)))])))]))

(define ((format-clause-2 pretty) clause)
  (match clause
    [(node _ (or "(" "[") (or ")" "]") #f #f xs)
     (alt
      ;; try to fit in one line; only when there are exactly two things
      #;[a b]
      (match/extract pretty xs #:as unfits tail
        [([-head #t] [-something #f])
         (match tail
           ['() (pretty-node #:adjust '("[" "]")
                             #:unfits unfits
                             clause
                             (try-indent #:n 0
                                         #:because-of (list -something)
                                         (hs-append (flat (pretty -head))
                                                    (pretty -something))))]
           [_ fail])]
        [#:else fail])

      ;; general case
      (pretty-node
       #:adjust '("[" "]")
       clause
       (try-indent #:n 0 #:because-of xs ((pretty-v-concat/kw pretty) xs))))]
    [_ (pretty clause)]))

(define ((format-binding-pairs pretty) bindings)
  (match bindings
    [(node _ _ _ #f #f xs)
     (pretty-node
      bindings
      (try-indent #:n 0
                  #:because-of xs
                  ;; try to fit in one line
                  (alt (flat (hs-concat (map (format-clause-2 pretty) xs)))
                       ;; general case
                       (v-concat (map (format-clause-2 pretty) xs)))))]
    [_ (pretty bindings)]))

(define ((format-node-if pretty) d)
  (((format-node-if-like format-node-#%app) pretty) d))

;; try to fit in one line if the body has exactly one form,
;; else will be multiple lines
#;(define x 1)
#;(define (y)
    a
    b)
(define (((format-node-define #:hook-for-head [hook-for-head values]) pretty) d)
  (match/extract pretty (node-content d) #:as unfits tail
    [([-define #t] [-head #f])
     ;; fit in one line case; only when there are exactly three things
     #;(define a b)
     (alt (pretty-node
           #:unfits unfits
           d
           (try-indent
            #:because-of tail
            (alt (match tail
                   [(list -e) (flat (hs-append (pretty -define)
                                               ((hook-for-head pretty) -head)
                                               (pretty -e)))]
                   [_ fail]))))

          ;; general case
          #;(define (a b)
              c
              d
              e)
          (((format-node-uniform-body 1 #:hook-for-arg hook-for-head) pretty)
           d))]
    [#:else ((format-node-#%app pretty) d)]))

(define format-node-let*
  (format-node-define #:hook-for-head format-binding-pairs))

;; support both named let and usual let
(define ((format-node-let pretty) d)
  (match/extract pretty (node-content d) #:as unfits tail
    ;; named let
    [([-let #t] [(? atom? -name) #t] [(? node? -bindings) #f])
     (pretty-node
      #:unfits unfits
      d
      (try-indent
       #:because-of tail
       (v-append (hs-append (pretty -let)
                            (pretty -name)
                            ((format-binding-pairs pretty) -bindings))
                 (h-append space (v-concat (map pretty tail))))))]
    ;; regular let
    [#:else ((format-node-let* pretty) d)]))

;; always in the form
#;(provide a
           b
           c)
(define ((format-node-require pretty) d)
  (match/extract pretty (node-content d) #:as unfits tail
    [([-provide #t] [-first-arg #f])
     (pretty-node
      #:unfits unfits
      d
      (hs-append (flat (pretty -provide))
                 (try-indent #:n 0
                             #:because-of (cons -first-arg tail)
                             (v-concat (map pretty (cons -first-arg tail))))))]
    [#:else ((format-node-#%app pretty) d)]))

;; support optional super id: either
#;(struct name super (fields ...)
    #:kw)
#;(struct name (fields ...)
    #:kw)
(define ((format-node-struct pretty) d)
  (match/extract pretty (node-content d) #:as unfits tail
    [([_ #t] [_ #t] [(? atom?) #f])
     (((format-node-uniform-body 3 #:require-body? #f) pretty) d)]
    [#:else (((format-node-uniform-body 2 #:require-body? #f) pretty) d)]))

(define (standard-formatter-map name)
  (case name
    [("if") format-node-if]
    [("provide" "require" "import" "export" "link" "rename") format-node-require]
    [("public" "private" "override" "inherit" "field" "init")
     format-node-require]

    [("define" "define-for-syntax" "define-values") (format-node-define)]
    [("define-syntax" "define-syntaxes" "define-values-for-syntax")
     (format-node-define)]
    [("λ" "lambda") (format-node-define)]

    [("let*" "let-values" "let*-values" "letrec" "letrec-values")
     format-node-let*]
    [("let-syntax" "letrec-syntax" "let-syntaxes" "letrec-syntaxes")
     format-node-let*]
    [("with-syntax" "with-syntax*") format-node-let*]
    [("shared") format-node-let*]
    [("parameterize" "parameterize*" "syntax-parameterize") format-node-let*]
    [("letrec-syntaxes+values")
     (format-node-uniform-body 2 #:hook-for-arg format-binding-pairs)]

    [("begin") (format-node-uniform-body 0)]
    [("begin0") (format-node-uniform-body 1)]
    [("module+") (format-node-uniform-body 1)]
    [("module" "module*") (format-node-uniform-body 2)]

    [("cond" "case-lambda") (format-node-uniform-body
                             0
                             #:hook-for-body format-clause-2
                             #:require-body? #f)]

    [("syntax-rules" "syntax-parse" "match" "match*" "case" "class")
     (format-node-uniform-body 1
                               #:hook-for-body format-clause-2
                               #:require-body? #f)]

    [("syntax-case" "instantiate")
     (format-node-uniform-body 2
                               #:hook-for-body format-clause-2
                               #:require-body? #f)]

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

    [("let") format-node-let]

    [("struct") format-node-struct]
    [("define-struct") (format-node-uniform-body 2 #:require-body? #f)]

    [else format-node-#%app]))
