#lang racket/base

(provide read-all
         (struct-out bare-prefix)
         (struct-out bare-sexp-comment))

(require racket/match
         racket/list
         syntax/readerr
         "common.rkt"
         "tokenize.rkt")

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

(define (read-one xs #:source [source #f] #:while-reading [while-reading #f])
  (match xs
    ['() (raise-read-eof-error "unexpected eof" source #f #f #f #f)]

    [(cons (token close-srcloc _ `(parenthesis ,(? close-paren? p))) _)
     (cond
       [while-reading (apply raise-read-error
                             (format "expected `~a` to close preceding `~a`, found instead `~a`"
                                     (find-closer while-reading)
                                     while-reading
                                     p)
                             source
                             close-srcloc)]
       [else (apply raise-read-error (format "unexpected `~a`" p) source close-srcloc)])]

    [(cons (token open-srcloc open-paren `(parenthesis ,(? open-paren? p))) xs)
     (define closer (find-closer p))
     (let loop ([xs xs] [acc '()])
       (match xs
         ['() (apply raise-read-eof-error
                     (format "expected a `~a` to close `~a`" closer p)
                     source
                     open-srcloc)]
         [(cons (token _ close-paren `(parenthesis ,(== closer))) xs)
          (process-tail (node #f open-paren close-paren #f #f (dropf (reverse (dropf acc nl?)) nl?))
                        xs)]

         [(cons (token _ _ `(white-space 0)) xs) (loop xs acc)]

         [(cons (token _ _ `(white-space 1)) xs) (loop xs acc)]

         [_
          (define-values (this xs*) (read-one xs #:source source #:while-reading p))
          (loop xs* (cons this acc))]))]

    [(cons (token _ _ `(white-space 0)) xs)
     (read-one xs #:source source #:while-reading while-reading)]

    [(cons (token _ _ `(white-space 1)) xs)
     (read-one xs #:source source #:while-reading while-reading)]

    [(cons (token _ _ `(white-space ,n)) xs) (values (nl (sub1 n)) xs)]

    [(cons (token _ (and c (or "'" "`" "#'" "#`")) 'constant) xs) (values (bare-prefix c) xs)]

    [(cons (token _ (and c (or "," ",@" "#," "#,@")) 'other) xs) (values (bare-prefix c) xs)]

    [(cons (token _ comment 'line-comment) xs) (values (line-comment comment) xs)]

    [(cons (token _ _ 'sexp-comment) xs) (values (bare-sexp-comment) xs)]

    [(cons (token _ content kind) xs) (process-tail (atom #f content kind) xs)]))

(define (read-top xs source)
  (let loop ([xs xs] [acc '()])
    (match xs
      ['() (reverse (dropf acc nl?))]
      [(cons (token _ _ `(white-space 0)) xs) (loop xs acc)]
      [(cons (token _ _ `(white-space 1)) xs) (loop xs acc)]
      [_
       (define-values (this xs*) (read-one xs #:source source))
       (loop xs* (cons this acc))])))

(define (read-all program-source max-blank-lines source)
  (read-top (tokenize program-source source max-blank-lines) source))
