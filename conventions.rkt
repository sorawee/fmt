#lang racket/base

(provide hook-standard)
(require racket/match
         racket/list
         pprint-compact
         "core.rkt"
         "conventions-util.rkt")

(define ((hook-app pretty) d)
  (match (node-content d)
    ['() (pretty-node d (list (app-format/no-comment empty-doc)))]
    [xs
     (define req-last-newline? (require-newline? (last xs)))
     (pretty-node
      d
      (cond
        [((current-app?) d)
         (append
          (match (extract* pretty xs (list #f))
            [#f (list (app-format/no-comment
                       (flush-if req-last-newline?
                                 ((pretty-v-concat/kw pretty) xs))))]
            [(list (list head) app-format tail)
             (list
              #;(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
                 #:xxx aaaaaaaaaaaaaaaaaaaaaaaaaaaaa
                 bbbbbbbbbbbbbbb
                 cccccccccccccccc)
              (app-format
               (flush-if req-last-newline?
                         ((pretty-v-concat/kw pretty)
                          (cons head tail)))))])
          (match (extract* pretty xs (list #t))
            [#f '()]
            [(list (list head) app-format tail)
             (list
              #;(a #:x a b c)
              (if (ormap require-newline? tail)
                  (app-format fail)
                  (app-format
                   (flat (hs-concat (map pretty (cons head tail))))))

              #;(aaaaaaaaaaaaa #:x aaaaaaaaaaaaaaa
                               bbbbbbbbbbbbbbb
                               cccccccccccccccc)
              (if (empty? tail)
                  app-format/fail
                  (app-format
                   (h-append
                    (flat (pretty head))
                    space
                    (flush-if req-last-newline?
                              ((pretty-v-concat/kw pretty)
                               tail))))))]))]
        [else
         (list (app-format/no-comment
                (flush-if req-last-newline?
                          (v-concat (map pretty xs))))
               (if (ormap require-newline? xs)
                   app-format/fail
                   (app-format/no-comment
                    (flat (hs-concat (map pretty xs))))))]))]))

(define ((hook-define-like pretty) d)
  (match (extract* pretty (node-content d) (list #t #f) #:at-least 3)
    ;; stuff like (define) and (define x) -- don't care
    [#f ((hook-app pretty) d)]
    ;; this is it!
    [(list (list -define -head) app-format tail)
     (pretty-node
      d
      (list
       ;; fit in one line case
       #;(define a b)
       (match tail
         [(list -e)
          #:when (not (or (require-newline? -head) (require-newline? -e)))
          (app-format (flat (hs-concat (list (pretty -define)
                                             (pretty -head)
                                             (pretty -e)))))]
         [_ (app-format fail)])

       ;; regular case
       #;(define (a b)
           c
           d
           e)
       (app-format
        (v-append
         (h-append (pretty -define) space (pretty -head))
         (h-append
          space
          (flush-if (require-newline? (last tail))
                    (v-concat (map pretty tail))))))))]))

(define ((hook-clause pretty) clause)
  (match clause
    [(node _ (or "(" "[") (or ")" "]") xs)
     (pretty-node
      #:adjust '("[" "]")
      clause
      (list (app-format/no-comment
             (match xs
               ['() empty-doc]
               [_ (flush-if (require-newline? (last xs))
                            ((pretty-v-concat/kw pretty) xs))]))
            (match (extract* pretty xs (list #t #f))
              [(list (list -head -something) app-format '())
               #:when (not (require-newline? -something))
               (app-format (hs-append (flat (pretty -head))
                                      (pretty -something)))]
              [_ app-format/fail])))]
    [_ (pretty clause)]))

(define (((hook-with-uniform-body
           n
           #:hook-for-arg [hook-for-arg values]
           #:hook-for-body [hook-for-body values])
          pretty)
         d)
  (match (extract* pretty
                   (node-content d)
                   (append (make-list n #t) (list #f)))
    ;; don't care
    [#f ((hook-app pretty) d)]
    ;; this is it!
    [(list (list -macro-name -e ...) app-format tail)
     (define args (map (hook-for-arg pretty) -e))
     (define first-line
       (hs-append (pretty -macro-name)
                  (alt (flat (hs-concat args))
                       (v-concat args))))
     (pretty-node
      d
      (list
       (match tail
         #;(match x)
         ['()
          (app-format
           (cond
             [(require-newline? (last (cons -macro-name -e)))
              (v-append first-line
                        space)]
             [else first-line]))]

         #;(match x
             [a b])
         [_
          (app-format
           (v-append
            first-line
            (h-append
             space
             (flush-if
              (require-newline? (last tail))
              (v-concat (map (hook-for-body pretty) tail))))))])))]))

(define ((hook-binding-pairs pretty) bindings)
  (match bindings
    [(? node?)
     (match (node-content bindings)
       ['() (pretty-node bindings (list (app-format/no-comment empty-doc)))]
       [xs
        (pretty-node
         bindings
         (list
          (cond
            ;; try to fit in one line
            [(not (ormap require-newline? xs))
             (app-format/no-comment (flat (hs-concat (map pretty xs))))]
            [else app-format/fail])
          ;; all separate lines
          (app-format/no-comment
           (flush-if (require-newline? (last xs))
                     (v-concat (map pretty xs))))))])]
    [_ (pretty bindings)]))

(define ((hook-let-like pretty) d)
  (match (extract* pretty (node-content d) (list #t #f) #:at-least 3)
    ;; stuff like (let) or (let ([x y])) -- don't care
    [#f (((hook-app) pretty) d)]
    [(list (list _ (? atom?)) _ _)
     (=> fail-pattern)
     (match (extract* pretty (node-content d) (list #t #t #f) #:at-least 4)
       [#f (fail-pattern)]
       [(list (list -let -name -bindings) app-format tail)
        (pretty-node
         d
         (list
          (app-format
           (v-append
            (hs-append (pretty -let)
                       (pretty -name)
                       ((hook-binding-pairs pretty) -bindings))
            (h-append
             space
             (flush-if (require-newline? (last tail))
                       (v-concat (map pretty tail))))))))])]
    [(list (list -let -bindings) app-format tail)
     (pretty-node
      d
      (list
       ;; try to fit in one line
       (match tail
         #;(let ([x y]) z)
         [(list -body)
          #:when (not (or (require-newline? -bindings)
                          (require-newline? -body)))
          (app-format (flat (hs-append (pretty -let)
                                       ((hook-binding-pairs pretty) -bindings)
                                       (pretty -body))))]
         [_ app-format/fail])

       #;(let ([x y])
           x
           y)
       (app-format
        (v-append
         (hs-append (pretty -let) ((hook-binding-pairs pretty) -bindings))
         (h-append
          space
          (flush-if (require-newline? (last tail))
                    (v-concat (map pretty tail))))))))]))

(define ((hook-require-like pretty) d)
  (match (extract* pretty (node-content d) (list #t))
    [(list (list -provide) app-format tail)
     (match tail
       ['() (pretty-node d (list (app-format/no-comment empty-doc)))]
       [_
        (pretty-node
         d
         (list
          (app-format
           (hs-append (pretty -provide)
                      (flush-if (require-newline? (last tail))
                                (v-concat (map pretty tail)))))))])]))

(define (hook-standard name)
  (case name
    ;; always in the form
    #;(provide a
               b
               c)
    [("provide" "require") hook-require-like]
    ;; try to fit in one line is the body has exactly one form,
    ;; else will be multiple lines
    #;(define x 1)
    #;(define (y)
        a
        b)
    [("define" "define-for-syntax" "define-values") hook-define-like]
    [("define-syntax" "define-syntaxes" "define-values-for-syntax")
     hook-define-like]
    [("λ" "lambda") hook-define-like]

    [("match" "match*" "case")
     (hook-with-uniform-body 1 #:hook-for-body hook-clause)]

    [("module+") (hook-with-uniform-body 1)]
    [("module" "module*") (hook-with-uniform-body 2)]

    [("cond" "case-lambda")
     (hook-with-uniform-body 0 #:hook-for-body hook-clause)]

    [("syntax-case") (hook-with-uniform-body 2 #:hook-for-body hook-clause)]

    [("syntax-rules") (hook-with-uniform-body 1 #:hook-for-body hook-clause)]

    [("syntax/loc" "quasisyntax/loc") (hook-with-uniform-body 1)]

    [("let*" "let-values" "let*-values" "letrec" "letrec-values")
     (hook-with-uniform-body 1 #:hook-for-arg hook-binding-pairs)]
    [("let-syntax" "letrec-syntax" "let-syntaxes" "letrec-syntaxes")
     (hook-with-uniform-body 1 #:hook-for-arg hook-binding-pairs)]
    [("with-syntax" "with-syntax*")
     (hook-with-uniform-body 1 #:hook-for-arg hook-binding-pairs)]
    [("parameterize" "parameterize*")
     (hook-with-uniform-body 1 #:hook-for-arg hook-binding-pairs)]

    [("when" "unless") (hook-with-uniform-body 1)]

    ;; these are really hacks... they don't support kws in body like #:break well
    [("for/fold" "for*/fold") (hook-with-uniform-body 2 #:hook-for-arg hook-binding-pairs)]
    [("for" "for*") (hook-with-uniform-body 1 #:hook-for-arg hook-binding-pairs)]
    [("for/list" "for*/list")
     (hook-with-uniform-body 1 #:hook-for-arg hook-binding-pairs)]
    [("for/hash" "for*/hash")
     (hook-with-uniform-body 1 #:hook-for-arg hook-binding-pairs)]
    [("for/hasheq" "for*/hasheq")
     (hook-with-uniform-body 1 #:hook-for-arg hook-binding-pairs)]
    [("for/hasheqv" "for*/hasheqv")
     (hook-with-uniform-body 1 #:hook-for-arg hook-binding-pairs)]
    [("for/vector" "for*/vector")
     (hook-with-uniform-body 1 #:hook-for-arg hook-binding-pairs)]

    ;; support both named let and usual let
    [("let") hook-let-like]
    [else hook-app]))
