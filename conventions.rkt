;; This file defines various styles for code formatting

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
         (except-in pretty-expressive flatten)
         "core.rkt"
         "common.rkt"
         "params.rkt"
         "record.rkt")

(define (default-kw-map _s _xs)
  1)

(define (syntax-parse-pattern-directive-kw-map s _xs)
  (case s
    [("#:declare" "#:with" "#:attr" "#:fail-when" "#:fail-unless") 2]
    [("#:cut") 0]
    [else 1]))

(define (syntax-parse-parse-option-kw-map s _xs)
  (case s
    [("#:track-literals" "#:disable-colon-notation") 0]
    [else 1]))

(define-pretty (format-kw-args -kw pos done format-kw-arg)
  #:type list?
  (let loop ([xs doc] [pos pos] [acc (list (pretty -kw))])
    (cond
      [(zero? pos) (done xs (reverse acc))]
      [else
       (match xs
         ['() (done xs (reverse acc))]
         [(cons x xs)
          (match x
            [(atom _ _ 'hash-colon-keyword) (done (cons x xs) (reverse acc))]
            [(? visible?) (loop xs (sub1 pos) (cons (format-kw-arg x) acc))]
            [_ (loop xs pos (cons (pretty x) acc))])])])))

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
        [_ (<$> x (loop xs))]))

    (match xs
      ['() empty-doc]
      [(list (and dot (atom _ "." 'other)) x (and another-dot (atom _ "." 'other)) xs ...)
       (define dot-formatted (pretty dot))
       (define x-formatted (format-body x))
       (define another-dot-formatted (pretty another-dot))
       (v-append-if (alt (flat (<s> dot-formatted x-formatted another-dot-formatted))
                         (<$> dot-formatted x-formatted another-dot-formatted))
                    xs)]
      [(list (and dot (atom _ "." 'other)) x xs ...)
       (define dot-formatted (pretty dot))
       (define x-formatted (format-body x))
       (v-append-if (<> dot-formatted (group nl) (align x-formatted))
                    xs)]
      [(list x (and ellipsis (atom _ (? (current-ellipsis?)) 'symbol)) xs ...)
       (define body-formatted (format-body x))
       (define ellipsis-formatted (pretty ellipsis))
       (v-append-if (<> body-formatted (group nl) ellipsis-formatted)
                    xs)]
      [(list (and kw (atom _ content 'hash-colon-keyword)) xs ...)
       ((format-kw-args kw
                        (kw-map content xs)
                        (λ (xs docs) (v-append-if (alt (v-concat docs) (as-concat docs)) xs))
                        format-kw-arg)
        xs)]
      [(list x xs ...) (v-append-if (format-body x) xs)])))

;; failable
(define-pretty (format-horizontal/helper #:body-formatter [format-body #f]
                                         #:kw-arg-formatter [format-kw-arg #f]
                                         #:kw-map [kw-map default-kw-map])
  #:type list?
  #:default [format-body pretty]
  #:default [format-kw-arg pretty]

  (flat (let loop ([xs doc])
          (define (h-append-if x xs)
            (match xs
              ['() x]
              [_ (<s> x (loop xs))]))

          (match xs
            ['() empty-doc]
            [(list (and kw (atom _ content 'hash-colon-keyword)) xs ...)
             ((format-kw-args kw
                              (kw-map content xs)
                              (λ (xs docs) (h-append-if (as-concat docs) xs))
                              format-kw-arg)
              xs)]
            [(list x xs ...) (h-append-if (format-body x) xs)]))))

(define-pretty (format-if-like/helper format-else
                                      #:expel-first-comment? [expel-first-comment? #t]
                                      #:adjust [adjust '("(" ")")])
  #:type node?
  (match/extract (node-content doc) #:as unfits tail
    [([-if expel-first-comment?] [-conditional #f])
     (pretty-node #:unfits unfits
                  #:adjust adjust
                  (<s> (flat (pretty -if))
                       (align (try-indent #:n 0
                                          #:because-of (cons -conditional tail)
                                          ;; multiple lines
                                          #;(if aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
                                                bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
                                                ccccccccccccccccccccccccccccccccc)
                                          (alt ((format-vertical/helper) (cons -conditional tail))
                                               ;; or one line
                                               #;(if a b c)
                                               (flat (as-concat (map pretty (cons -conditional tail)))))))))]
    [#:else (format-else doc)]))

(define-pretty format-#%app
  #:type node?
  #:let [xs (filter-not newl? (node-content doc))]
  #:let [doc (struct-copy node doc [content xs])]
  (cond
    [((current-app?) doc)
     (match/extract xs #:as unfits tail
       ;; mostly vertical
       [([-head #f])
        (alt (cost '(0 0 1)
                   (pretty-node #:unfits unfits
                                #:adjust #f
                                (try-indent #:n 0
                                            #:because-of (cons -head tail)
                                            ((format-vertical/helper) (cons -head tail)))))
             (match tail
               [(list (atom _ (? (current-ellipsis?)) 'symbol) _ ...)
                (pretty-node #:unfits unfits
                             #:adjust #f
                             (try-indent #:n 0
                                         #:because-of (cons -head tail)
                                         ((format-horizontal/helper) (cons -head tail))))]
               [_
                ;; pretty cases
                ((format-if-like/helper #:expel-first-comment? #f #:adjust #f (λ (d) fail)) doc)]))]
       ;; perhaps full of comments, or there's nothing at all
       [#:else (pretty-node #:adjust #f
                            (try-indent #:n 0 #:because-of xs ((format-vertical/helper) xs)))])]
    [else
     (pretty-node #:adjust #f
                  (try-indent #:n 0
                              #:because-of xs
                              ;; general case
                              (alt ((format-vertical/helper) xs)
                                   ;; try to fit in one line
                                   (flat (as-concat (map pretty xs))))))]))

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
          (<s> (pretty -macro-name) (align (alt (v-concat args) (flat (as-concat args)))))]))
     (pretty-node
      #:unfits unfits
      (try-indent
       #:because-of (append (cons -macro-name -e) tail)
       (match tail
         ['()
          #:when (not require-body?)
          first-line]
         [_
          (<$> first-line
               (<> space
                   (align ((format-vertical/helper #:body-formatter format-body #:kw-map kw-map)
                           tail))))])))]))

(define-pretty (format-clause-2/indirect #:kw-map [kw-map default-kw-map] #:flat? [flat? #t])
  #:type values
  (match doc
    [(node _ (or "(" "[") (or ")" "]") #f #f xs)
     ;; general case
     (alt (cost '(0 0 1)
                (pretty-node
                 #:adjust '("[" "]")
                 (try-indent #:n 0 #:because-of xs ((format-vertical/helper #:kw-map kw-map) xs))))
          ;; try to fit in one line; only when there are exactly two things
          #;[a b]
          (match/extract xs #:as unfits tail
            [([-head #t] [-something #f])
             (match tail
               ['()
                (pretty-node #:adjust '("[" "]")
                             #:unfits unfits
                             (try-indent #:n 0
                                         #:because-of (list -something)
                                         (let ([line (<s> (flat (pretty -head)) (align (pretty -something)))])
                                           (if flat? (flat line) line))))]
               [_ fail])]
            [#:else fail]))]
    [_ (pretty doc)]))

(define-pretty format-binding-pairs/indirect
  #:type values
  (match doc
    [(node _ _ _ #f #f xs)
     (pretty-node
      (try-indent
       #:n 0
       #:because-of xs
       ;; general case
       (alt ((format-vertical/helper #:body-formatter (format-clause-2/indirect #:flat? #f)) xs)
            ;; try to fit in one line
            ((format-horizontal/helper #:body-formatter (format-clause-2/indirect #:flat? #f)) xs))))]
    [_ (pretty doc)]))

(define format-if (format-if-like/helper format-#%app))

;; try to fit in one line if the body has exactly one form,
;; else will be multiple lines
;; for one line, and when it's a function, always format vertically.
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
     ;; general case
     #;(define (a b)
         c
         d
         e)
     (alt ((format-uniform-body/helper 1 #:arg-formatter format-head) doc)
          ;; fit in one line case; only when there are either two or three things
          #;(define a b)
          (pretty-node
           #:unfits unfits
           (try-indent
            #:because-of tail
            (flat (match -head
                    [(? node?) fail]
                    [_
                     (match tail
                       ['() (<s> (pretty -define) (format-head -head))]
                       [(list -e) (<s> (pretty -define) (format-head -head) (pretty -e))]
                       [_ fail])])))))]
    [#:else (format-#%app doc)]))

;; try to fit in one line if the body has exactly one form,
;; else will be multiple lines
#;(define-values (xxxxxxxxxxx yyyyyyyyyyy) 1)
#;(define-values (xxxxxxxxxxx yyyyyyyyyyy)
    11111111111111111111111111111111111111111111111111111111111111111111111)
(define-pretty (format-define-like #:head-formatter [format-head #f])
  #:type node?
  #:default [format-head pretty]
  (match/extract (node-content doc) #:as unfits tail
    [([-define #t] [-head #f])
     ;; general case
     #;(define (a b)
         c
         d
         e)
     (alt ((format-uniform-body/helper 1 #:arg-formatter format-head) doc)
          ;; fit in one line case; only when there are either two or three things
          #;(define a b)
          (pretty-node
           #:unfits unfits
           (try-indent
            #:because-of tail
            (flat (match tail
                    ['() (<s> (pretty -define) (format-head -head))]
                    [(list -e) (<s> (pretty -define) (format-head -head) (pretty -e))]
                    [_ fail])))))]
    [#:else (format-#%app doc)]))

;; this is similar to let*, but because the macro name is so long,
;; also allow the second form to be in its own line
(define-pretty format-parameterize
  #:type node?
  (match/extract (node-content doc) #:as unfits tail
    [([-parameterize #f] [-bindings #f])
     (define bindings (align (format-binding-pairs/indirect -bindings)))
     (<> (pretty-node
          #:unfits unfits
          (<> (pretty -parameterize)
              (alt (cost '(0 0 3) (<> nl (text "   ") bindings))
                   (<> (text " ") bindings))
              nl
              space
              (align (try-indent #:because-of tail ((format-vertical/helper) tail))))))]
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
     (pretty-node #:unfits unfits
                  (try-indent #:because-of tail
                              (<$> (<s> (pretty -let)
                                        (pretty -name)
                                        (align (format-binding-pairs/indirect -bindings)))
                                   (<> space (align (v-concat (map pretty tail)))))))]
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
                  (<s> (flat (pretty -provide))
                       (align (try-indent #:n 0
                                          #:because-of (cons -first-arg tail)
                                          ((format-vertical/helper) (cons -first-arg tail))))))]
    [#:else (format-#%app doc)]))

;; support optional super id: either
#;(struct name super (fields ...) #:kw)
#;(struct name (fields ...) #:kw)
(define-pretty format-struct
  #:type node?
  (match/extract (node-content doc) #:as unfits tail
    [([-struct #t] [-name #t] [(? atom? -super) #f])
     (alt ((format-uniform-body/helper 3 #:require-body? #f) doc)
          (pretty-node (try-indent #:because-of (cons -super tail)
                                   ((format-horizontal/helper) (list* -struct -name -super tail)))))]
    [([-struct #t] [-name #t] [-fields #f])
     (alt ((format-uniform-body/helper 2 #:require-body? #f) doc)
          (pretty-node (try-indent #:because-of (cons -fields tail)
                                   ((format-horizontal/helper) (list* -struct -name -fields tail)))))]
    [#:else (format-#%app doc)]))

;; some for forms support keyword arguments before the clauses
#;(for/vector #:length 10
              ([i (in-range 10)])
    i)
#;(for/vector #:length 10 #:fill 20
              ([i (in-range 10)])
    i)
#;(for/list/concurrent #:group (make-thread-group)
                       ([i (in-range 10)])
    i)
(define-pretty (format-for-like n)
  #:type node?
  (match/extract (node-content doc) #:as unfits tail
    [([-for-name #t] [(and (atom _ _ 'hash-colon-keyword) -kwd) #t])
     (define-values (kwds groups body)
       (let loop ([kwds null] [groups null] [tail (cons -kwd tail)])
         (match tail
           [(list (and (atom _ _ 'hash-colon-keyword) -kwd) -kwd-arg -tail* ...)
            #:when (null? groups)
            (loop (cons (cons -kwd -kwd-arg) kwds) groups -tail*)]
           [(list (? node? -group) -tail* ...)
            #:when (< (length groups) n)
            (loop kwds (cons -group groups) -tail*)]
           [_
            (values (apply <s>
                           (for/list ([p (in-list (reverse kwds))])
                             (<s> (align (pretty (car p))) (align (pretty (cdr p))))))
                    (for/list ([g (in-list (reverse groups))])
                      (format-binding-pairs/indirect g))
                    (<> space (align ((format-vertical/helper) tail))))])))

     (define first-line
       (<s> (pretty -for-name)
            (align (<$> kwds (alt (v-concat groups) (flat (as-concat groups)))))))
     (pretty-node #:unfits unfits
                  (try-indent #:because-of (list* -for-name -kwd tail) (<$> first-line body)))]
    [#:else ((format-uniform-body/helper n #:arg-formatter format-binding-pairs/indirect) doc)]))

(define/record standard-formatter-map #:record all-kws
  [("if") format-if]
  [("provide" "require" "import" "export" "link" "rename") format-require]
  [("public" "private" "override" "augment" "inherit" "field" "init") format-require]
  [("pubment" "public-final" "overment" "override-final" "augride" "augment-final") format-require]

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

  [("define/contract") (format-define-like)]

  [("let*") format-let*]
  [("let-values" "let*-values" "letrec" "letrec-values") format-parameterize]
  [("let-syntax" "letrec-syntax" "let-syntaxes" "letrec-syntaxes") format-parameterize]
  [("with-syntax" "with-syntax*" "with-handlers" "with-handlers*" "shared") format-parameterize]
  [("parameterize" "parameterize*" "syntax-parameterize") format-parameterize]
  [("match-let" "match-let*" "match-let-values" "match-let*-values") format-parameterize]
  [("match-letrec" "match-letrec-values") format-parameterize]

  [("letrec-syntaxes+values")
   (format-uniform-body/helper 2 #:arg-formatter format-binding-pairs/indirect)]

  [("splicing-let" "splicing-letrec" "splicing-let-values") format-parameterize]
  [("splicing-letrec-values" "splicing-let-syntax" "splicing-letrec-syntax") format-parameterize]
  [("splicing-let-syntaxes" "splicing-letrec-syntaxes" "splicing-letrec-syntaxes+values")
   format-parameterize]
  [("splicing-parameterize" "splicing-syntax-parameterize") format-parameterize]

  [("begin" "begin-for-syntax") (format-uniform-body/helper 0 #:require-body? #f)]
  [("test-begin") (format-uniform-body/helper 0 #:require-body? #f)]
  [("begin0") (format-uniform-body/helper 1)]
  [("module+") (format-uniform-body/helper 1)]
  [("test-case" "test-suite") (format-uniform-body/helper 1)]
  [("define-syntax-class" "define-match-expander") (format-uniform-body/helper 1)]
  [("class") (format-uniform-body/helper 1 #:require-body? #f)]
  [("interface") (format-uniform-body/helper 1 #:require-body? #f)]
  [("interface*") (format-uniform-body/helper 2 #:require-body? #f)]
  [("module" "module*") (format-uniform-body/helper 2)]

  [("cond" "case-lambda" "match-lambda" "match-lambda*" "match-lambda**")
   (format-uniform-body/helper 0 #:body-formatter (format-clause-2/indirect) #:require-body? #f)]

  [("syntax-rules" "match" "match*" "case" "define/match")
   (format-uniform-body/helper 1 #:body-formatter (format-clause-2/indirect) #:require-body? #f)]

  [("define-syntax-parse-rule" "define-simple-macro")
   (format-uniform-body/helper 1 #:kw-map syntax-parse-pattern-directive-kw-map)]

  [("pattern")
   (format-uniform-body/helper 1 #:kw-map syntax-parse-pattern-directive-kw-map #:require-body? #f)]

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
  [("for/fold" "for*/fold") (format-for-like 2)]
  [("for" "for*") (format-for-like 1)]
  [("for/list" "for*/list") (format-for-like 1)]
  [("for/and" "for*/and" "for/or" "for*/or") (format-for-like 1)]
  [("for/first" "for*/first" "for/last" "for*/last") (format-for-like 1)]
  [("for/hash" "for*/hash") (format-for-like 1)]
  [("for/hasheq" "for*/hasheq") (format-for-like 1)]
  [("for/hasheqv" "for*/hasheqv") (format-for-like 1)]
  [("for/vector" "for*/vector") (format-for-like 1)]
  [("for/async" "for*/async") (format-for-like 1)]
  [("for/list/concurrent" "for*/list/concurrent") (format-for-like 1)]

  [("let") format-let]

  [("struct") format-struct]
  [("define-struct") (format-uniform-body/helper 2 #:require-body? #f)]

  [else format-#%app])
