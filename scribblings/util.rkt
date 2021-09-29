#lang racket/base

(provide external-file
         external-file/format
         compare)

(require racket/include
         racket/match
         racket/list
         scribble/manual
         scribble/struct
         (only-in scribble/core table-columns table-cells style plain
                  color-property nested-flow)
         scribble/html-properties
         (for-label racket/base)
         (for-syntax racket/base
                     racket/port
                     fmt))

;; Taken from pkgs/racket-doc/scribblings/guide/contracts/utils.rkt from
;; the Racket repo

(require (for-syntax (only-in scribble/comment-reader
                              [read-syntax comment-reader])))


(define (sty columns width #:valign? [valign? #t])
  (define space
    (style #f `(,(attributes `((width . ,(format "~a" width))
                               (align . "left")
                               ,@(if valign?
                                     (list '(valign . "top"))
                                     (list)))))))
  ;; -- in --
  (style #f
         (list
          (attributes '((border . "1") (cellpadding . "1")))
          (table-columns (make-list columns space)))))

(define stretching-style
  (style #f (list (attributes '([style . "margin-left: 0; margin-right: 0"])))))

(define (extract d)
  (match d
    [(nested-flow _ (list (nested-flow _ content)))
     (nested-flow stretching-style content)]
    [_ d]))

(define (compare stuff1 stuff2)
  (define stuff (list (list (extract stuff1)) (list (extract stuff2))))
  (table (sty 2 500) (apply map (compose make-flow list) stuff)))

(define-for-syntax ((comment-racketmod-reader transform name) path port*)
  (define port (transform port*))
  (let ([pb (peek-byte port)])
    (if (eof-object? pb)
        pb
        (let ([m (regexp-match #rx"^#lang " port)])
          (unless m
            (raise-syntax-error
             'comment-racket-reader
             "expected a #lang to begin file ~s"
             path))
          (let ([np (let-values ([(line col pos) (port-next-location port)])
                      (relocate-input-port port line 0 pos))])
            (port-count-lines! np)
            (let loop ([objects '()])
              (let ([next (comment-reader path np)])
                (cond
                  [(eof-object? next)
                   #`(racketmod #:file (tt #,name)
                                #,@(reverse objects))]
                  [else (loop (cons next objects))]))))))))

(define-for-syntax (comment-racketmod-reader/format n)
  (comment-racketmod-reader
   (λ (p)
     (define code (port->string p))
     (define p*
       (open-input-string
        (program-format code standard-formatter-map #:width 40)))
     (port-count-lines! p*)
     p*)
   n))

(define-for-syntax (comment-racketmod-reader/no-format n)
  (comment-racketmod-reader values n))

(define-syntax (external-file stx)
  (syntax-case stx ()
    [(_ filename #:name n)
     #`(include/reader #,(format "~a" (syntax-e #'filename))
                       (comment-racketmod-reader/no-format n))]))

(define-syntax (external-file/format stx)
  (syntax-case stx ()
    [(_ filename #:name n)
     (begin
       #`(include/reader #,(format "~a" (syntax-e #'filename))
                         (comment-racketmod-reader/format n)))]))
