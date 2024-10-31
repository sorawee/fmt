#lang racket

(provide (contract-out [a integer?]
                       [b integer?]
                       [c integer?]))

(provide a
         b
         c
         (contract-out [a integer?]
                       [b integer?]
                       [c integer?]))

(provide
  glob/c
  (contract-out
    [glob (->* [glob/c] [#:capture-dotfiles? boolean?] (listof path?))]
    [in-glob (->* [glob/c] [#:capture-dotfiles? boolean?] (sequence/c path?))]
    [glob-match? (->* [glob/c path-string?] [#:capture-dotfiles? boolean?] boolean?)]
    [glob-quote (->i ([ps path-string?]) [r (ps) (if (path? ps) path? string?)])]
    [glob-capture-dotfiles? (parameter/c boolean?)]))

(provide
 (contract-out
  [untgz (->* ((or/c path-string? input-port?))
              (#:dest
               (or/c #f path-string?)
               #:strip-count exact-nonnegative-integer?
               #:permissive? any/c
               #:filter (path? (or/c path? #f)
                               symbol? exact-integer? (or/c path? #f)
                               exact-nonnegative-integer? exact-nonnegative-integer?
                               . -> . any/c))
              void?)]))

(begin-for-syntax
  (require racket/contract/base
           syntax/parse/private/pattern-expander
           (submod syntax/parse/private/residual ct))
  (provide pattern-expander?
           (contract-out
            [prop:syntax-class
             (struct-type-property/c (or/c identifier? (-> any/c identifier?)))]
            [pattern-expander
             (-> (-> syntax? syntax?) pattern-expander?)]
            [prop:pattern-expander
             (struct-type-property/c (-> pattern-expander? (-> syntax? syntax?)))]
            [syntax-local-syntax-parse-pattern-introduce
             (-> syntax? syntax?)])))

(provide (contract-out
          [make-constructor-style-printer
           (-> (-> any/c (or/c symbol? string?))
               (-> any/c sequence?)
               (-> any/c output-port? (or/c #t #f 0 1) void?))])
         struct->list)


(provide parse-srv-rr
         (contract-out
          (struct srv-rr ((priority (integer-in 0 65535))
                          (weight (integer-in 0 65535))
                          (port (integer-in 0 65535))
                          (target string?)))))

(provide (contract-out [crypto-random-bytes (-> exact-nonnegative-integer? bytes?)]
                       [random-ref (->* (sequence?) (pseudo-random-generator?) any/c)]
                       [random-sample (->* (sequence? exact-nonnegative-integer?)
                                           (pseudo-random-generator?
                                            #:replacement? any/c)
                                           (listof any/c))]))

  (provide
   (contract-out
    [argmax
      (->i ([f (-> any/c real?)] [lov (and/c pair? list?)]) ()
	(r (f lov)
           (lambda (r)
             (define f@r (f r))
             (for/and ((v lov)) (>= f@r (f v))))))]))