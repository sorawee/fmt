#lang racket/base

(provide tokenize
         (struct-out token))

(require racket/match
         racket/list
         racket/string
         syntax-color/module-lexer)

(struct token (srcloc text type) #:transparent)

;; tokenize :: string? natural-number/c any/c -> (listof token?)
(define (tokenize program-source max-blank-lines source)
  (define max-newlines (add1 max-blank-lines))
  (define p (open-input-string program-source source))
  (port-count-lines! p)
  (let loop ([mode #f])
    (define start-srcloc (call-with-values (λ () (port-next-location p)) list))
    (match-define-values (text type paren-type start-pos end-pos _ new-mode)
      #;(module-lexer* p 0 mode)
      (module-lexer p 0 mode))
    (cond
      [(eof-object? text) '()]
      [else
       (define srcloc
         (list (first start-srcloc) (second start-srcloc) (third start-srcloc) (- end-pos start-pos)))
       (define current
         (cond
           [(eq? type 'parenthesis) (token srcloc text `(parenthesis ,paren-type))]
           [(eq? type 'white-space)
            (define num-newlines (sub1 (length (string-split text "\n" #:trim? #f))))
            (token srcloc
                   ""
                   `(white-space ,(cond
                                    [(> num-newlines max-newlines) max-newlines]
                                    [else num-newlines])))]

           [(eq? type 'sexp-comment)
            (define re-read (substring program-source (sub1 start-pos) (sub1 end-pos)))
            (cond
              [(equal? text re-read) (token srcloc text 'sexp-comment)]
              ;; this is sexp comment before #lang, treat it as a block comment
              [else (token srcloc re-read 'block-comment)])]

           ;; non-comment
           [(not (eq? type 'comment)) (token srcloc text type)]
           ;; non-empty regular line comment
           [(non-empty-string? text)
            (define re-read (substring program-source (sub1 start-pos) (sub1 end-pos)))
            (token srcloc re-read 'line-comment)]
           ;; empty regular line comment
           [(= end-pos (add1 start-pos)) (token srcloc ";" 'line-comment)]
           ;; block comment
           [else (token srcloc
                        (substring program-source (sub1 start-pos) (sub1 end-pos))
                        'block-comment)]))
       (cons current (loop new-mode))])))

(module+ main
  (tokenize "#lang racket/base
a
#;#;(abc) def
qwe" 1 #f))
