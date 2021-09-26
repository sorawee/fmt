#lang racket/base

(provide program-format
         (struct-out thing)
         (struct-out node)
         (struct-out atom)
         (struct-out nl)
         (struct-out line-comment)
         (struct-out sexp-comment)
         (struct-out wrapper)
         (struct-out toplevel)
         pretty-comment
         extract
         visible?)

(require racket/match
         racket/string
         racket/list
         racket/function
         syntax/readerr
         syntax-color/module-lexer
         pprint-compact
         pprint-compact/memoize
         #;pprint-compact/debug
         #;(only-in racket/pretty [pretty-print pp]))

(struct token (srcloc text type) #:transparent)

(struct thing (extra) #:transparent)
(struct node thing (opener closer content) #:transparent)
(struct atom thing (content type) #:transparent)
;; invariant: n >= 1
(struct nl thing (n) #:transparent)
(struct line-comment thing (content) #:transparent)
(struct toplevel thing (content) #:transparent)
;; these two only exist after the realign pass
(struct sexp-comment thing (style tok content) #:transparent)
(struct wrapper thing (tk content) #:transparent)

;; these two will be removed by the realign pass
(struct bare-prefix thing (tok) #:transparent)
(struct bare-sexp-comment thing () #:transparent)


(define (strip-comment obj)
  (if (thing-extra obj)
      (cond
        [(node? obj)
         (struct-copy node obj
                      [extra #:parent thing #f])]
        [(atom? obj)
         (struct-copy atom obj
                      [extra #:parent thing #f])]
        [(wrapper? obj)
         (struct-copy wrapper obj
                      [extra #:parent thing #f])]
        [(sexp-comment? obj)
         (struct-copy sexp-comment obj
                      [extra #:parent thing #f])]
        [else obj])
      obj))

(define (check-left? xs at-least)
  (cond
    [(zero? at-least) #t]
    [else
     (match xs
       ['() #f]
       [(cons x xs)
        (cond
          [(visible? x) (check-left? xs (sub1 at-least))]
          [else (check-left? xs at-least)])])]))

(define (extract xs
                 extract-configs
                 #:at-least [at-least (length extract-configs)])
  (let loop ([xs xs]
             [extract-configs extract-configs]
             [fits '()]
             [unfits '()]
             [at-least at-least])
    (match extract-configs
      ['() (and (check-left? xs at-least)
                (list (reverse fits)
                      (filter (λ (x) (not (nl? x))) (reverse unfits))
                      xs))]
      [(cons extract-config extract-configs)
       (match xs
         ['() #f]
         [(cons x xs)
          (cond
            [(visible? x)
             (cond
               [(and extract-config (thing-extra x))
                (loop xs
                      extract-configs
                      (cons (strip-comment x) fits)
                      (cons (line-comment #f (thing-extra x)) unfits)
                      (sub1 at-least))]
               [else
                (loop xs
                      extract-configs
                      (cons x fits)
                      unfits
                      (sub1 at-least))])]
            [else (loop xs
                        (cons extract-config extract-configs)
                        fits
                        (cons x unfits)
                        at-least)])])])))

;; tokenize :: string? -> (listof token?)
(define (tokenize program-source
                  #:max-newlines [max-newlines 2]
                  #:source [source #f])
  (define p (open-input-string program-source source))
  (port-count-lines! p)
  (let loop ([mode #f])
    (define start-srcloc (call-with-values (λ () (port-next-location p)) list))
    (match-define-values (text type paren-type start-pos end-pos _ new-mode)
      (module-lexer p 0 mode))
    (cond
      [(eof-object? text) '()]
      [else
       (define srcloc (list (first start-srcloc)
                            (second start-srcloc)
                            (third start-srcloc)
                            (- end-pos start-pos)))
       (define current
         (cond
           [(eq? type 'parenthesis)
            (token srcloc text `(parenthesis ,paren-type))]
           [(eq? type 'white-space)
            (define num-newlines
              (sub1 (length (string-split text "\n" #:trim? #f))))
            (token srcloc
                   ""
                   `(white-space ,(cond
                                    [(> num-newlines max-newlines) max-newlines]
                                    [else num-newlines])))]

           [(eq? type 'sexp-comment)
            (define re-read
              (substring program-source (sub1 start-pos) (sub1 end-pos)))
            (cond
              [(equal? text re-read)
               (token srcloc text 'sexp-comment)]
              [else
               ;; this is sexp comment before #lang, treat it as a block comment
               (token srcloc re-read 'block-comment)])]

           ;; non-comment
           [(not (eq? type 'comment))
            (token srcloc text type)]
           ;; non-empty regular line comment
           [(non-empty-string? text)
            (token srcloc (string-append ";" text) 'line-comment)]
           ;; empty regular line comment
           [(= end-pos (add1 start-pos))
            (token srcloc ";" 'line-comment)]
           ;; block comment
           [else
            (token srcloc
                   (substring program-source (sub1 start-pos) (sub1 end-pos))
                   'block-comment)]))
       (cons current (loop new-mode))])))

(define (find-closer p)
  (match p
    ['|(| '|)|]
    ['|[| '|]|]
    ['|{| '|}|]))

(define openers (list '|(| '|[| '|{|))
(define closers (list '|)| '|]| '|}|))

(define (open-paren? x) (memq x openers))
(define (close-paren? x) (memq x closers))

;; precondition: obj is either node or atom
(define (process-tail obj xs)
  (define (do-it comment xs)
    (values
     (cond
       [(node? obj)
        (struct-copy node obj
                     [extra #:parent thing comment])]
       [(atom? obj)
        (struct-copy atom obj
                     [extra #:parent thing comment])])
     xs))
  (match xs
    [(list (token _ _ `(white-space 0)) (token _ comment 'line-comment) xs ...)
     (do-it comment xs)]
    [(list (token _ comment 'line-comment) xs ...)
     (do-it comment xs)]
    [_ (values obj xs)]))

(define (read-one xs
                  #:source [source #f]
                  #:while-reading [while-reading #f])
  (match xs
    ['() (raise-read-eof-error "unexpected eof" source #f #f #f #f)]

    [(cons (token close-srcloc _ `(parenthesis ,(? close-paren? p))) _)
     (cond
       [while-reading
        (apply raise-read-error
               (format
                "expected `~a` to close preceding `~a`, found instead `~a`"
                (find-closer while-reading)
                while-reading
                p)
               source
               close-srcloc)]
       [else
        (apply raise-read-error
               (format "unexpected `~a`" p)
               source
               close-srcloc)])]

    [(cons (token open-srcloc open-paren `(parenthesis ,(? open-paren? p))) xs)
     (define closer (find-closer p))
     (let loop ([xs xs] [acc '()])
       (match xs
         ['() (apply raise-read-eof-error
                     (format "expected a `~a` to close `~a`" closer p)
                     source
                     open-srcloc)]
         [(cons (token _ close-paren `(parenthesis ,(== closer))) xs)
          (process-tail (node #f
                              open-paren
                              close-paren
                              (dropf (reverse (dropf acc nl?)) nl?))
                        xs)]

         [(cons (token _ _ `(white-space 0)) xs)
          (loop xs acc)]

         [(cons (token _ _ `(white-space 1)) xs)
          (loop xs acc)]

         [_
          (define-values (this xs*)
            (read-one xs #:source source #:while-reading p))
          (loop xs* (cons this acc))]))]

    [(cons (token _ _ `(white-space 0)) xs)
     (read-one xs #:source source #:while-reading while-reading)]

    [(cons (token _ _ `(white-space 1)) xs)
     (read-one xs #:source source #:while-reading while-reading)]

    [(cons (token _ _ `(white-space ,n)) xs)
     (values (nl #f (sub1 n)) xs)]

    [(cons (token _ (and c (or "'" "`" "#'" "#`")) 'constant) xs)
     (values (bare-prefix #f c) xs)]

    [(cons (token _ (and c (or "," ",@" "#," "#,@")) 'other) xs)
     (values (bare-prefix #f c) xs)]

    [(cons (token _ comment 'line-comment) xs)
     (values (line-comment #f comment) xs)]

    [(cons (token _ _ 'sexp-comment) xs)
     (values (bare-sexp-comment #f) xs)]

    [(cons (token _ content kind) xs)
     (process-tail (atom #f content kind) xs)]))

(define (read-top xs #:source [source #f])
  (let loop ([xs xs] [acc '()])
    (match xs
      ['()
       (toplevel #f (reverse (dropf acc nl?)))]

      [(cons (token _ _ `(white-space 0)) xs)
       (loop xs acc)]

      [(cons (token _ _ `(white-space 1)) xs)
       (loop xs acc)]

      [_
       (define-values (this xs*) (read-one xs #:source source))
       (loop xs* (cons this acc))])))

(define (visible? x)
  (or (node? x) (atom? x) (wrapper? x)))

(define (realign/seq xs)
  (let loop ([xs xs] [just-read-sexp-comment? #f])
    (match xs
      ['() '()]
      [(cons (? nl? d) xs)
       (cons d (loop xs #f))]
      [(cons (? atom? d) xs)
       (cons d (loop xs #f))]
      [(cons (? line-comment? d) xs)
       (cons d (loop xs #f))]
      [(cons (? bare-sexp-comment?) xs)
       (define-values (invisibles tail)
         (splitf-at (loop (dropf xs nl?) #t) (negate visible?)))
       (match tail
         ['() (raise-read-error "sexp-comment without content" #f #f #f #f #f)]
         [(cons visible xs)
          (match invisibles
            [(cons (sexp-comment comment style? tok content) invisibles)
             (append
              (list (sexp-comment comment style? (string-append "#;" tok) content))
              invisibles
              (cons visible xs))]
            ['()
             #:when (not just-read-sexp-comment?)
             (match visible
               [(node comment opener closer content)
                (cons (node comment (string-append "#;" opener) closer content)
                      xs)]
               [_
                (cons
                 (sexp-comment (thing-extra visible)
                               'any
                               "#;"
                               (list (strip-comment visible)))
                 xs)])]
            [_
             (cons
              (sexp-comment (thing-extra visible)
                            'newline
                            "#;"
                            (append invisibles (list (strip-comment visible))))
              xs)])])]

      [(cons (bare-prefix _ tk) xs)
       (define-values (invisibles tail)
         (splitf-at (loop (dropf xs nl?) #f) (negate visible?)))
       (match tail
         ['() (raise-read-error "quote without content" #f #f #f #f #f)]
         [(cons visible xs)
          (append
           invisibles
           (cons (match visible
                   ;; don't create a new wrapper, just transfer content
                   [(wrapper comment tk* content)
                    (wrapper comment (string-append tk tk*) content)]
                   [(node comment opener closer content)
                    (node comment (string-append tk opener) closer content)]
                   [_ (wrapper (thing-extra visible)
                               tk
                               (strip-comment visible))])
                 xs))])]
      [(cons (node comment opener closer xs*) xs)
       (cons (node comment opener closer (loop xs* #f)) (loop xs #f))])))

(define (realign d)
  (match d
    [(toplevel comment xs)
     (toplevel comment (realign/seq xs))]))

(define (pretty-comment comment d)
  (if comment
      (full (hs-append d (text comment)))
      d))

(define (pretty d hook)
  (define loop
    (memoize
     (λ (d)
       (match d
         [(toplevel _ xs) (v-concat (map loop xs))]
         [(nl _ n) (full (v-concat (make-list n empty-doc)))]
         [(atom comment content _) (pretty-comment comment (text content))]
         [(line-comment _ comment) (full (text comment))]
         [(sexp-comment comment style tok xs)
          (pretty-comment
           comment
           (match style
             ['newline (v-append (text tok)
                                 (v-concat (map loop xs)))]
             ['any
              (define xs* (v-concat (map loop xs)))
              (alt (h-append (text tok) xs*)
                   (v-append (text tok) xs*))]))]
         [(node _ _ _ xs)
          (match (extract xs (list #f))
            [#f (((hook #f) loop) d)]
            [(list (list (atom _ content 'symbol)) _ _)
             (((hook content) loop) d)]
            [_ (((hook #f) loop) d)])]
         [(wrapper comment tok content)
          (pretty-comment comment
                          (h-append (text tok) (loop content)))]))))
  (loop d))

;; program-format :: string? -> string?
(define (program-format program-source
                        #:source [source #f]
                        #:width [width 80]
                        #:hook [hook (λ (name) #f)])
  (define doc
    (pretty (realign (read-top (tokenize program-source #:source source)
                               #:source source))
            hook))
  (pretty-format doc #:width width))
