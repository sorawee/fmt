#lang racket/base

(provide format-code)

(require racket/string
         racket/match
         racket/port
         racket/runtime-path
         syntax/parse
         "config.rkt")

(define-runtime-module-path config.rkt "config.rkt")

;; read-directive :: string? string? -> (or/c #f config?)
;; inputting:
;;   content: the whole program source
;;   source-path: source path
;; outputting:
;;   - #f: there is no file-based directive
;;   - config?: otherwise
(define (read-directive content source-path)
  (define p (open-input-string content source-path))
  (port-count-lines! p)
  (parameterize ([read-accept-lang #t]
                 [read-accept-reader #t])
    (let loop ()
      (define peeking-p (peeking-input-port p))
      (port-count-lines! peeking-p)
      (define v (read/recursive peeking-p))
      (cond
        [(special-comment? v)
         (match-define-values (_ _ pos) (port-next-location peeking-p))
         (match (read-string (sub1 pos) (peeking-input-port p))
           [(pregexp #px"^\\s*#;(#\\[\\[(?s:.)*\\]\\])$" (list _ inside))
            (read-string (- pos (string-length inside) 1) p)
            (syntax-parse (read-syntax/recursive source-path p)
              #:datum-literals (fmt config)
              [#[[fmt {~and cfg (config _ ...)}]] #'cfg]
              [#[[fmt _ ...]]
               (raise-syntax-error #f "malformed fmt directive" this-syntax)]
              [#[[_ ...]] (loop)])]
           [_ #f])]
        [else #f]))))

(define (strip-trailing-spaces s)
  (string-join (for/list ([line (in-list (string-split s "\n"))])
                 (string-trim line #:left? #f))
               "\n"))

(define ns (make-base-namespace))
(parameterize ([current-namespace ns])
  (define this-ns (variable-reference->namespace (#%variable-reference)))
  (namespace-attach-module this-ns config.rkt)
  (namespace-require config.rkt))

(define (read-config-from-language content source-path)
  (define get-info
    (read-language (open-input-string content source-path)
                   (λ () #f)))
  (cond
    [get-info (get-info 'fmt:config #f)]
    [else #f]))

(define (format-code content
                     #:source-path [source-path #f]
                     #:setup [setup (λ (thk) (thk))])
  (define (do-format content)
    (define stx (read-directive content source-path))
    (define config-from-language (read-config-from-language content source-path))
    (define config-from-directive
      (cond
        [(syntax? stx)
         (parameterize ([current-namespace ns])
           (eval stx))]
        [else #f]))

    (with-config config-from-language
      (setup
       (λ ()
         (with-config config-from-directive
           (define content*
             (match (:selection)
               [(cons start end) (substring content start end)]
               [_ content]))
           (cond
             [(:enabled?)
              (define formatter (:formatter))
              (unless formatter
                (raise-user-error 'format-code "formatter is not given"))

              (define s (formatter content* #:source-path source-path))
              (cond
                [(:strip-trailing-spaces?) (strip-trailing-spaces s)]
                [else s])]
             [else #f]))))))

  ;; NOTE: a hack to preserve the first three lines that DrRacket hackily
  ;; recognizes and omits
  ;;
  ;; See https://github.com/sorawee/fmt/issues/39

  (match (string-split content "\n")
    [(list first-line second-line third-line rest-lines ...)
     #:when (and (string-prefix? first-line ";;")
                 (string-prefix? second-line ";;")
                 (string-prefix? third-line "#reader"))
     (define result (do-format (string-join rest-lines "\n")))
     (cond
       [result
        (string-join (list first-line second-line third-line result) "\n")]
       [else #f])]
    [_ (do-format content)]))
