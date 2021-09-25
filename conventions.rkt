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
              #;(a
                 #:x a
                 b
                 c)
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

              #;(a #:x a
                   b
                   c)
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

(define ((hook-define pretty) d)
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

(define ((hook-match pretty) d)
  (match (extract* pretty (node-content d) (list #t #f))
    ;; stuff like (match) -- don't care
    [#f ((hook-app pretty) d)]
    ;; this is it!
    [(list (list -match -e) app-format tail)
     (pretty-node
      d
      (list
       (match tail
         #;(match x)
         ['()
          (app-format
           (cond
             [(require-newline? -e)
              (v-append (hs-append (pretty -match) (pretty -e))
                        space)]
             [else (hs-append (pretty -match) (pretty -e))]))]

         #;(match x
             [a b])
         [_
          (app-format
           (v-append
            (hs-append (pretty -match) (pretty -e))
            (h-append
             space
             (flush-if
              (require-newline? (last tail))
              (v-concat (map (hook-clause pretty) tail))))))])))]))

(define ((hook-cond pretty) d)
  (match (extract* pretty (node-content d) (list #f))
    ;; this is it!
    [(list (list -cond) app-format tail)
     (pretty-node
      d
      (list
       (app-format
        (match tail
          #;(cond)
          ['() (if (require-newline? -cond)
                   (v-append (pretty -cond)
                             space)
                   (pretty -cond))]
          #;(cond
              [a b])
          [_ (v-append
              (pretty -cond)
              (h-append
               space
               (flush-if (require-newline? (last tail))
                         (v-concat (map (hook-clause pretty)
                                        tail)))))]))))]))


(define ((hook-let-bindings pretty) bindings)
  (match bindings
    [(? node?)
     (match (node-content bindings)
       ['() (pretty-node bindings (list (app-format/no-comment empty-doc)))]
       [xs
        (pretty-node
         bindings
         (list
          (cond
            [(not (ormap require-newline? xs))
             (app-format/no-comment (flat (hs-concat (map pretty xs))))]
            [else app-format/fail])
          (app-format/no-comment
           (flush-if (require-newline? (last xs))
                     (v-concat (map pretty xs))))))])]
    [_ (pretty bindings)]))

(define ((hook-let pretty) d)
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
                       ((hook-let-bindings pretty) -bindings))
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
                                       ((hook-let-bindings pretty) -bindings)
                                       (pretty -body))))]
         [_ app-format/fail])

       (app-format
        (v-append
         (hs-append (pretty -let) ((hook-let-bindings pretty) -bindings))
         (h-append
          space
          (flush-if (require-newline? (last tail))
                    (v-concat (map pretty tail))))))))]))

(define (hook-standard name)
  (case name
    [("define") hook-define]
    [("match") hook-match]
    [("cond") hook-cond]
    [("let") hook-let]
    [else hook-app]))
