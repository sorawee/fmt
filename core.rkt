#lang racket/base

(provide program-format
         (struct-out thing)
         (struct-out node)
         (struct-out atom)
         (struct-out nl)
         (struct-out line-comment)
         (struct-out sexp-comment)
         (struct-out bare-sexp-comment)
         (struct-out toplevel)
         (struct-out wrapper)
         require-newline?
         pretty-node)

(require racket/match
         racket/string
         racket/list
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
(struct nl thing (n) #:transparent)
(struct line-comment thing (content) #:transparent)
(struct sexp-comment thing (level content) #:transparent)
(struct bare-sexp-comment thing (tok) #:transparent)
(struct toplevel thing (content) #:transparent)
(struct wrapper thing (prefix content) #:transparent)

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
              [else (token srcloc re-read 'block-comment)])]

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

(define (read-one xs #:source [source #f] #:while-reading [while-reading #f])
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
     (define-values (this xs*)
       (read-one xs #:source source #:while-reading #f))
     (values (wrapper #f c this) xs*)]

    [(cons (token _ (and c (or "," ",@" "#," "#,@")) 'other) xs)
     (define-values (this xs*)
       (read-one xs #:source source #:while-reading #f))
     (values (wrapper #f c this) xs*)]

    [(cons (token _ comment 'line-comment) xs)
     (values (line-comment #f comment) xs)]

    [(cons (token _ tk 'sexp-comment) xs)
     (match xs
       [(cons (token _ _ `(white-space ,n)) xs)
        #:when (positive? n)
        (values (bare-sexp-comment #f tk) xs)]
       [_
        (define-values (this xs*)
          (read-one xs #:source source #:while-reading #f))
        (values (match this
                  [(bare-sexp-comment _ tk*)
                   (bare-sexp-comment #f (string-append tk tk*))]
                  [(sexp-comment _ level content)
                   (sexp-comment #f (add1 level) content)]
                  [_ (sexp-comment #f 1 this)])
                xs*)])]

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

(define sexp-comment-tok (text "#;"))

(define (require-newline? d)
  (or (thing-extra d)
      (line-comment? d)
      (bare-sexp-comment? d)
      (nl? d)))

(define (pretty-node n d)
  (match-define (node comment opener closer _) n)
  (define main-doc (h-append (text opener) d (text closer)))
  (if comment
      (h-append main-doc space (text comment))
      main-doc))

(define ((pretty hook) d)
  (define loop
    (memoize
     (λ (d)
       (match d
         [(toplevel _ xs) (v-concat (map loop xs))]
         [(nl _ n) (v-concat (make-list n empty-doc))]
         [(atom comment content _)
          (if comment
              (text (string-append content " " comment))
              (text content))]
         [(line-comment _ comment) (text comment)]
         [(sexp-comment _ 1 (line-comment _ comment))
          (text (string-append "#; " comment))]
         [(sexp-comment _ 1 content)
          (define content* (loop content))
          (alt (h-append sexp-comment-tok content*)
               (v-append sexp-comment-tok content*))]
         [(sexp-comment _ n content)
          (define content* (loop content))
          (v-append (h-concat (make-list n sexp-comment-tok))
                    content*)]
         [(bare-sexp-comment _ tok) (text tok)]
         [(wrapper _ pre (line-comment _ comment))
          (text (string-append pre " " comment))]
         [(wrapper _ pre content) (h-append (text pre) (loop content))]
         [(node _ _ _ xs)
          (define (default)
            (define xs* (map loop xs))
            (define req-last-newline? (require-newline? (last xs)))
            (alt
             (flush-if req-last-newline? (v-concat xs*))
             (if (ormap require-newline? xs)
                 fail
                 (flat (hs-concat xs*)))
             (if (require-newline? (first xs))
                 fail
                 (h-append (flat (first xs*))
                           space
                           (flush-if req-last-newline? (v-concat (rest xs*)))))))
          (match xs
            ['() (pretty-node d empty-doc)]
            ;; TODO: checking the first token is not ideal, but will do it for now
            [(cons (atom _ content 'symbol) _)
             (define proc (hook content))
             (cond
               [proc (pretty-node d ((proc loop) xs))]
               [else (pretty-node d (default))])]
            [_ (pretty-node d (default))])]))))
  (loop d))

;; program-format :: string? -> string?
(define (program-format program-source
                        #:source [source #f]
                        #:width [width 80]
                        #:hook [hook (λ (name) #f)])
  (define doc
    ((pretty hook)
     (read-top (tokenize program-source #:source source)
               #:source source)))
  (pretty-format doc #:width width))
