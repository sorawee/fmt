#lang racket/base

(provide standard-formatter-map

         format-vertical/helper
         format-horizontal/helper
         format-if-like/helper

         format-#%app

         format-uniform-body/helper

         format-clause-2/indirect
         format-binding-pairs/indirect

         format-define
         format-define-like
         format-let
         format-let*
         format-require
         format-if
         format-struct

         syntax-parse-pattern-directive-kw-map
         syntax-parse-parse-option-kw-map
         default-kw-map

         all-kws)

(require racket/match
         racket/list
         pprint-compact
         "core.rkt"
         "common.rkt"
         "params.rkt"
         "record.rkt")

(define (default-kw-map _s _xs) 1)

(define (syntax-parse-pattern-directive-kw-map s _xs)
  (case s
    [("#:declare" "#:with" "#:attr" "#:fail-when" "#:fail-unless") 2]
    [("#:cut") 0]
    [else 1]))

(define (syntax-parse-parse-option-kw-map s _xs)
  (case s
    [("#:track-literals" "#:disable-colon-notation") 0]
    [else 1]))

(define-pretty (format-vertical/helper #:body-formatter [format-body #f]
                                       #:kw-arg-formatter [format-kw-arg #f]
                                       #:kw-map [kw-map default-kw-map])
  #:type list?
  #:default [format-body pretty]
  #:default [format-kw-arg pretty]

  (let loop ([xs doc])
    (define (v-append-if x xs)
      (match xs
        ['() x]
        [_ (v-append x (loop xs))]))

    (match xs
      ['() empty-doc]
      [(list (and dot (atom _ "." 'other)) x (and another-dot (atom _ "." 'other)) xs ...)
       (v-append-if (alt (hs-append (pretty dot) (format-body x) (pretty another-dot))
                         (v-append (pretty dot) (format-body x) (pretty another-dot)))
                    xs)]
      [(list (and dot (atom _ "." 'other)) x xs ...)
       (v-append-if
        (alt (hs-append (pretty dot) (format-body x)) (v-append (pretty dot) (format-body x)))
        xs)]
      [(list x (and ellipsis (atom _ (? (current-ellipsis?)) 'symbol)) xs ...)
       (v-append-if (alt (hs-append (format-body x) (pretty ellipsis))
                         (v-append (format-body x) (pretty ellipsis)))
                    xs)]
      [(list (and kw (atom _ content 'hash-colon-keyword)) xs ...)
       (define pos (kw-map content xs))
       (define (do-it xs docs)
         (v-append-if (alt (hs-concat docs) (v-concat docs)) xs))
       (let loop2 ([xs xs] [left pos] [acc (list (pretty kw))])
         (cond
           [(zero? left) (do-it xs (reverse acc))]
           [else (match xs
                   ['() (do-it xs (reverse acc))]
                   [(cons x xs) (cond
                                  [(visible? x) (loop2 xs (sub1 left) (cons (format-kw-arg x) acc))]
                                  [else (loop2 xs left (cons (pretty x) acc))])])]))]
      [(list x xs ...) (v-append-if (format-body x) xs)])))

;; failable
(define-pretty (format-horizontal/helper #:body-formatter [format-body #f]
                                         #:kw-arg-formatter [format-kw-arg #f]
                                         #:kw-map [kw-map default-kw-map])
  #:type list?
  #:default [format-body pretty]
  #:default [format-kw-arg pretty]

  (flat
   (let loop ([xs doc])
     (define (h-append-if x xs)
       (match xs
         ['() x]
         [_ (hs-append x (loop xs))]))

     (match xs
       ['() empty-doc]
       [(list (and kw (atom _ content 'hash-colon-keyword)) xs ...)
        (define pos (kw-map content xs))
        (define (do-it xs docs)
          (h-append-if (hs-concat docs) xs))
        (let loop2 ([xs xs] [left pos] [acc (list (pretty kw))])
          (cond
            [(zero? left) (do-it xs (reverse acc))]
            [else (match xs
                    ['() (do-it xs (reverse acc))]
                    [(cons x xs) (cond
                                   [(visible? x) (loop2 xs (sub1 left) (cons (format-kw-arg x) acc))]
                                   [else (loop2 xs left (cons (pretty x) acc))])])]))]
       [(list x xs ...) (h-append-if (format-body x) xs)]))))

(define-pretty (format-if-like/helper format-else)
  #:type node?
  (match/extract (node-content doc) #:as unfits tail
    [([-if #t] [-conditional #f])
     (pretty-node #:unfits unfits
                  (hs-append (flat (pretty -if))
                             (try-indent #:n 0
                                         #:because-of (cons -conditional tail)
                                         ;; try to fit in one line
                                         #;(if a b c)
                                         (alt (flat (hs-concat (map pretty (cons -conditional tail))))
                                              ;; or multiple lines
                                              #;(if aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
                                                    bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
                                                    ccccccccccccccccccccccccccccccccc)
                                              ((format-vertical/helper) (cons -conditional tail))))))]
    [#:else (format-else doc)]))

(define-pretty format-#%app
  #:type node?
  #:let [xs (filter-not nl? (node-content doc))]
  #:let [doc (struct-copy node doc [content xs])]
  (cond
    [((current-app?) doc)
     (match/extract xs #:as unfits tail
       ;; mostly vertical
       [([-head #f]) (alt (pretty-node #:unfits unfits
                                       (try-indent #:n 0
                                                   #:because-of (cons -head tail)
                                                   ((format-vertical/helper) (cons -head tail))))
                          ;; pretty cases
                          ((format-if-like/helper (λ (d) fail)) doc))]
       ;; perhaps full of comments, or there's nothing at all
       [#:else (pretty-node (try-indent #:n 0 #:because-of xs ((format-vertical/helper) xs)))])]
    [else (pretty-node (try-indent #:n 0
                                   #:because-of xs
                                   ;; general case
                                   (alt ((format-vertical/helper) xs)
                                        ;; try to fit in one line
                                        (flat (hs-concat (map pretty xs))))))]))

(define-pretty (format-uniform-body/helper n
                                           #:arg-formatter [format-arg #f]
                                           #:body-formatter [format-body #f]
                                           #:require-body? [require-body? #t]
                                           #:kw-map [kw-map default-kw-map])
  #:type node?
  #:default [format-arg pretty]
  #:default [format-body pretty]
  (match (extract (node-content doc) (append (make-list n #t) (list #f)))
    ;; don't care
    [#f (format-#%app doc)]
    ;; this is it!
    [(list (list -macro-name -e ...) unfits tail)
     (define first-line
       (match -e
         ['() (pretty -macro-name)]
         [_
          (define args (map format-arg -e))
          (hs-append (pretty -macro-name) (alt (flat (hs-concat args)) (v-concat args)))]))
     (pretty-node
      #:unfits unfits
      (try-indent
       #:because-of (append (cons -macro-name -e) tail)
       (match tail
         ['()
          #:when (not require-body?)
          first-line]
         [_ (v-append first-line
                      (h-append space
                                ((format-vertical/helper #:body-formatter format-body #:kw-map kw-map)
                                 tail)))])))]))

(define-pretty (format-clause-2/indirect #:kw-map [kw-map default-kw-map])
  #:type values
  (match doc
    [(node _ (or "(" "[") (or ")" "]") #f #f xs)
     ;; try to fit in one line; only when there are exactly two things
     #;[a b]
     (alt (match/extract xs #:as unfits tail
            [([-head #t] [-something #f])
             (match tail
               ['() (pretty-node #:adjust '("[" "]")
                                 #:unfits unfits
                                 (try-indent #:n 0
                                             #:because-of (list -something)
                                             (hs-append (flat (pretty -head)) (pretty -something))))]
               [_ fail])]
            [#:else fail])
          ;; general case
          (pretty-node
           #:adjust '("[" "]")
           (try-indent #:n 0 #:because-of xs ((format-vertical/helper #:kw-map kw-map) xs))))]
    [_ (pretty doc)]))

(define-pretty format-binding-pairs/indirect
  #:type values
  (match doc
    [(node _ _ _ #f #f xs)
     (pretty-node
      (try-indent #:n 0
                  #:because-of xs
                  ;; try to fit in one line
                  (alt ((format-horizontal/helper #:body-formatter (format-clause-2/indirect)) xs)
                       ;; general case
                       ((format-vertical/helper #:body-formatter (format-clause-2/indirect)) xs))))]
    [_ (pretty doc)]))

(define format-if (format-if-like/helper format-#%app))

(define (low-width m)
  (<= (measure-last-width m) (current-inline-limit)))

;; try to fit in one line if the body has exactly one form,
;; else will be multiple lines
;; for one line, and when it's a function, make sure the body is not too long
;; (according to low-width)
#;(define x 1)
#;(define (y)
    a
    b)
#;(define (foo)
    111111111111111111111111111111111)
(define-pretty (format-define #:head-formatter [format-head #f])
  #:type node?
  #:default [format-head pretty]
  (match/extract (node-content doc) #:as unfits tail
    [([-define #t] [-head #f])
     ;; fit in one line case; only when there are exactly three things
     #;(define a b)
     (alt
      (pretty-node
       #:unfits unfits
       (try-indent
        #:because-of tail
        (alt (match tail
               [(list -e) (match -head
                            [(? node?) (flat (hs-append (pretty -define)
                                                        (format-head -head)
                                                        (select (pretty -e) low-width)))]
                            [_ (flat (hs-append (pretty -define) (format-head -head) (pretty -e)))])]
               [_ fail]))))
      ;; general case
      #;(define (a b)
          c
          d
          e)
      ((format-uniform-body/helper 1 #:arg-formatter format-head) doc))]
    [#:else (format-#%app doc)]))

;; try to fit in one line if the body has exactly one form,
;; else will be multiple lines
#;(define x 1)
#;(define (y)
    a
    b)
(define-pretty (format-define-like #:head-formatter [format-head #f])
  #:type node?
  #:default [format-head pretty]
  (match/extract (node-content doc) #:as unfits tail
    [([-define #t] [-head #f])
     ;; fit in one line case; only when there are exactly three things
     #;(define a b)
     (alt (pretty-node
           #:unfits unfits
           (try-indent
            #:because-of tail
            (alt (match tail
                   [(list -e) (flat (hs-append (pretty -define) (format-head -head) (pretty -e)))]
                   [_ fail]))))
          ;; general case
          #;(define (a b)
              c
              d
              e)
          ((format-uniform-body/helper 1 #:arg-formatter format-head) doc))]
    [#:else (format-#%app doc)]))

;; this is similar to let*, but because the macro name is so long,
;; also allow the second form to be in its own line
(define-pretty format-parameterize
  #:type node?
  (match/extract (node-content doc) #:as unfits tail
    [([-parameterize #f] [-bindings #f])
     (alt (cost (pretty-node
                 #:unfits unfits
                 (v-append
                  (pretty -parameterize)
                  (h-append (text "   ") (format-binding-pairs/indirect -bindings))
                  (h-append space (try-indent #:because-of tail ((format-vertical/helper) tail)))))
                1)
          (format-let* doc))]
    [#:else (format-#%app doc)]))

;; unlike format-node-parameterize, don't allow the second form to be in its
;; own line, even though it is technically better. It just looks really ugly.
(define format-let* (format-define-like #:head-formatter format-binding-pairs/indirect))

;; support both named let and usual let
(define-pretty format-let
  #:type node?
  (match/extract (node-content doc) #:as unfits tail
    ;; named let
    [([-let #t] [(? atom? -name) #t] [(? node? -bindings) #f])
     (pretty-node
      #:unfits unfits
      (try-indent
       #:because-of tail
       (v-append (hs-append (pretty -let) (pretty -name) (format-binding-pairs/indirect -bindings))
                 (h-append space (v-concat (map pretty tail))))))]
    ;; regular let
    [#:else (format-let* doc)]))

;; always in the form
#;(provide a
           b
           c)
(define-pretty format-require
  #:type node?
  (match/extract (node-content doc) #:as unfits tail
    [([-provide #t] [-first-arg #f])
     (pretty-node #:unfits unfits
                  (hs-append (flat (pretty -provide))
                             (try-indent #:n 0
                                         #:because-of (cons -first-arg tail)
                                         (v-concat (map pretty (cons -first-arg tail))))))]
    [#:else (format-#%app doc)]))

;; support optional super id: either
#;(struct name super (fields ...)
    #:kw)
#;(struct name (fields ...)
    #:kw)
(define-pretty format-struct
  #:type node?
  (match/extract (node-content doc) #:as unfits tail
    [([_ #t] [_ #t] [(? atom?) #f]) ((format-uniform-body/helper 3 #:require-body? #f) doc)]
    [#:else ((format-uniform-body/helper 2 #:require-body? #f) doc)]))

(define/record (standard-formatter-map name) #:record all-kws
  (case name
    [("if") format-if]
    [("provide" "require" "import" "export" "link" "rename") format-require]
    [("public" "private" "override" "inherit" "field" "init") format-require]

    [("define") (format-define)]
    [("define-for-syntax" "define-values") (format-define-like)]
    [("define-syntax-rule") (format-define-like)]
    [("define-syntax" "define-syntaxes" "define-values-for-syntax") (format-define-like)]
    [("define-syntax-parameter") (format-define-like)]
    [("define/public" "define/private" "define/override" "define/augment") (format-define-like)]
    [("define/pubment" "define/augride" "define/overment") (format-define-like)]
    [("define/public-final" "define/override-final" "define/augment-final") (format-define-like)]

    [("λ" "lambda") (format-define-like)]
    [("match-define" "match-define-values") (format-define-like)]

    [("let*") format-let*]
    [("let-values" "let*-values" "letrec" "letrec-values") format-parameterize]
    [("let-syntax" "letrec-syntax" "let-syntaxes" "letrec-syntaxes") format-parameterize]
    [("with-syntax" "with-syntax*" "with-handlers" "with-handlers*" "shared") format-parameterize]
    [("parameterize" "parameterize*" "syntax-parameterize") format-parameterize]
    [("letrec-syntaxes+values")
     (format-uniform-body/helper 2 #:arg-formatter format-binding-pairs/indirect)]

    [("splicing-let" "splicing-letrec" "splicing-let-values") format-parameterize]
    [("splicing-letrec-values" "splicing-let-syntax" "splicing-letrec-syntax") format-parameterize]
    [("splicing-let-syntaxes" "splicing-letrec-syntaxes" "splicing-letrec-syntaxes+values")
     format-parameterize]
    [("splicing-parameterize" "splicing-syntax-parameterize") format-parameterize]

    [("begin" "begin-for-syntax") (format-uniform-body/helper 0 #:require-body? #f)]
    [("begin0") (format-uniform-body/helper 1)]
    [("module+") (format-uniform-body/helper 1)]
    [("define-syntax-class") (format-uniform-body/helper 1)]
    [("class") (format-uniform-body/helper 1 #:require-body? #f)]
    [("module" "module*") (format-uniform-body/helper 2)]

    [("cond" "case-lambda")
     (format-uniform-body/helper 0 #:body-formatter (format-clause-2/indirect) #:require-body? #f)]

    [("syntax-rules" "match" "match*" "case")
     (format-uniform-body/helper 1 #:body-formatter (format-clause-2/indirect) #:require-body? #f)]

    [("define-syntax-parse-rule" "define-simple-macro")
     (format-uniform-body/helper 1 #:kw-map syntax-parse-pattern-directive-kw-map)]

    [("syntax-parse" "define-syntax-parser")
     (format-uniform-body/helper
      1
      #:require-body? #f
      #:body-formatter (format-clause-2/indirect #:kw-map syntax-parse-pattern-directive-kw-map)
      #:kw-map syntax-parse-parse-option-kw-map)]

    [("syntax-parser")
     (format-uniform-body/helper
      0
      #:require-body? #f
      #:body-formatter (format-clause-2/indirect #:kw-map syntax-parse-pattern-directive-kw-map)
      #:kw-map syntax-parse-parse-option-kw-map)]

    [("syntax-case" "instantiate")
     (format-uniform-body/helper 2 #:body-formatter (format-clause-2/indirect) #:require-body? #f)]

    [("syntax/loc" "quasisyntax/loc") (format-uniform-body/helper 1)]
    [("when" "unless") (format-uniform-body/helper 1)]

    [("mixin") (format-uniform-body/helper 2)]
    [("for/fold" "for*/fold")
     (format-uniform-body/helper 2 #:arg-formatter format-binding-pairs/indirect)]
    [("for" "for*") (format-uniform-body/helper 1 #:arg-formatter format-binding-pairs/indirect)]
    [("for/list" "for*/list")
     (format-uniform-body/helper 1 #:arg-formatter format-binding-pairs/indirect)]
    [("for/and" "for*/and" "for/or" "for*/or")
     (format-uniform-body/helper 1 #:arg-formatter format-binding-pairs/indirect)]
    [("for/first" "for*/first" "for/last" "for*/last")
     (format-uniform-body/helper 1 #:arg-formatter format-binding-pairs/indirect)]
    [("for/hash" "for*/hash")
     (format-uniform-body/helper 1 #:arg-formatter format-binding-pairs/indirect)]
    [("for/hasheq" "for*/hasheq")
     (format-uniform-body/helper 1 #:arg-formatter format-binding-pairs/indirect)]
    [("for/hasheqv" "for*/hasheqv")
     (format-uniform-body/helper 1 #:arg-formatter format-binding-pairs/indirect)]
    [("for/vector" "for*/vector")
     (format-uniform-body/helper 1 #:arg-formatter format-binding-pairs/indirect)]

    [("let") format-let]

    [("struct") format-struct]
    [("define-struct") (format-uniform-body/helper 2 #:require-body? #f)]

    [else format-#%app]))
