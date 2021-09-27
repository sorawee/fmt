#lang racket/base

(provide external-file
         do-format)
(require racket/include
         scribble/manual
         (for-label racket/base)
         (for-syntax racket/base
                     racket/port
                     racket/system))

;; Taken from pkgs/racket-doc/scribblings/guide/contracts/utils.rkt from
;; the Racket repo

(require (for-syntax (only-in scribble/comment-reader
                              [read-syntax comment-reader])))
(define-for-syntax (comment-racketmod-reader path port)
  (define-values (_base name _must-be-dir?) (split-path path))
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
                   #`(racketmod #:file #,(path->string name)
                                #,@(reverse objects))]
                  [else
                   (loop (cons next objects))]))))))))

(define-syntax (external-file stx)
  (syntax-case stx ()
    [(_ filename)
     #`(include/reader #,(format "~a" (syntax-e #'filename))
                       comment-racketmod-reader)]))

(define-syntax (do-format stx)
  (syntax-case stx ()
    [(_ src dest)
     (begin
       (system (format "raco fmt ~a > ~a" (syntax-e #'src) (syntax-e #'dest)))
       #`(include/reader #,(format "~a" (syntax-e #'dest))
                         comment-racketmod-reader))]))
