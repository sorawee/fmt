;; The read pass

#lang racket/base

(provide read-all
         (struct-out bare-prefix)
         (struct-out bare-sexp-comment))

(require racket/match
         racket/list
         racket/string
         syntax/readerr
         "common.rkt"
         "tokenize.rkt")

(define current-source (make-parameter #f))

;; these two will be removed by the realign pass
(struct bare-prefix thing (tok) #:transparent)
(struct bare-sexp-comment thing () #:transparent)

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
    (values (cond
              [(node? obj) (struct-copy node obj [inline-comment #:parent commentable comment])]
              [(atom? obj) (struct-copy atom obj [inline-comment #:parent commentable comment])])
            xs))
  (match xs
    [(list (token _ _ `(white-space 0)) (token _ comment 'line-comment) xs ...) (do-it comment xs)]
    [(list (token _ comment 'line-comment) xs ...) (do-it comment xs)]
    [_ (values obj xs)]))

(struct done (val) #:transparent)

(define (read-one xs
                  #:on-closer
                  [on-closer
                   (λ (close-srcloc p text xs)
                     (apply raise-read-error
                            (format "unexpected `~a`" text)
                            (current-source)
                            close-srcloc))]
                  #:on-eof
                  [on-eof
                   (λ () (raise-read-eof-error "unexpected eof"
                                               (current-source)
                                               #f
                                               #f
                                               #f
                                               #f))])
  (match xs
    ['() (on-eof)]

    [(cons (token close-srcloc text `(parenthesis ,(? close-paren? p))) xs)
     (on-closer close-srcloc p text xs)]

    [(cons (token open-srcloc open-paren `(parenthesis ,(? open-paren? p))) xs)
     (define closer (find-closer p))
     (let loop ([xs xs] [acc '()])
       (define-values (this xs*)
         (read-one xs
                   #:on-closer
                   (λ (closer-srcloc closer-sym closer-text xs)
                     (cond
                       [(equal? closer-sym closer)
                        (define-values (this xs*)
                          (process-tail
                           (node #f open-paren closer-text #f #f (dropf (reverse (dropf acc newl?)) newl?))
                           xs))
                        (values (done this) xs*)]
                       [else
                        (apply raise-read-error
                               (format "expected `~a` to close preceding `~a`, found instead `~a`"
                                       closer
                                       p
                                       closer-text)
                               (current-source)
                               closer-srcloc)]))
                   #:on-eof
                   (λ ()
                     (apply raise-read-eof-error
                            (format "expected a `~a` to close `~a`" closer p)
                            (current-source)
                            open-srcloc))))

       (cond
         [(done? this) (values (done-val this) xs*)]
         [else (loop xs* (cons this acc))]))]

    [(cons (token _ _ `(white-space 0)) xs)
     (read-one xs #:on-eof on-eof #:on-closer on-closer)]

    [(cons (token _ _ `(white-space 1)) xs)
     (read-one xs #:on-eof on-eof #:on-closer on-closer)]

    [(cons (token _ _ `(white-space ,n)) xs) (values (newl (sub1 n)) xs)]

    [(cons (token _ (and c (or "'" "`" "#'" "#`")) 'constant) xs) (values (bare-prefix c) xs)]

    [(cons (token _ (and c (or "," ",@" "#," "#,@")) 'other) xs) (values (bare-prefix c) xs)]

    [(cons (token _ comment 'line-comment) xs) (values (line-comment comment) xs)]

    [(cons (token _ _ 'sexp-comment) xs) (values (bare-sexp-comment) xs)]

    [(cons (token _ content 'string) xs)
     #:when (string-prefix? content "#<<")
     (process-tail (full-atom #f content 'string) xs)]

    [(cons (token _ content kind) xs) (process-tail (atom #f content kind) xs)]))

(define (read-top xs)
  (let loop ([xs xs] [acc '()])
    (define-values (this xs*) (read-one xs #:on-eof (λ () (values 'eof '()))))
    (cond
      [(eq? 'eof this) (reverse (dropf acc newl?))]
      [else (loop xs* (cons this acc))])))

(define (read-all program-source max-blank-lines source)
  (define toks (tokenize program-source source max-blank-lines))
  (parameterize ([current-source source])
    (read-top toks)))
