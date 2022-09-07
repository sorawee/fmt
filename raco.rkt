#lang racket/base

; don't run this file for testing:
(module test racket/base)

(require racket/cmdline
         racket/file
         racket/list
         racket/match
         racket/string
         (rename-in "main.rkt"
                    [current-width :current-width]
                    [current-max-blank-lines :current-max-blank-lines]
                    [current-indent :current-indent]))

(define current-width
  (make-parameter
   (:current-width)
   (λ (n)
     (define as-num (string->number n))
     (cond
       [(and as-num (or (exact-nonnegative-integer? as-num) (= as-num +inf.0)))
        as-num]
       [else (raise-user-error
              'width
              "must be either a natural number or +inf.0, given: ~a"
              n)]))))

(define current-max-blank-lines
  (make-parameter
   (:current-max-blank-lines)
   (λ (n)
     (define as-num (string->number n))
     (cond
       [(and as-num (or (exact-nonnegative-integer? as-num) (= as-num +inf.0)))
        as-num]
       [else (raise-user-error
              'max-blank-lines
              "must be either a natural number or +inf.0, given: ~a"
              n)]))))

(define current-indent
  (make-parameter
   (:current-indent)
   (λ (n)
     (define as-num (string->number n))
     (cond
       [(and as-num (exact-nonnegative-integer? as-num))
        as-num]
       [else (raise-user-error
              'indent
              "must be either a natural number, given: ~a"
              n)]))))

(define current-config (make-parameter "-"))
(define current-in-place? (make-parameter #f))

(define filenames
  (command-line
   #:once-each
   [("--width")
    w
    "page width limit -- must be a natural number or +inf.0 (default: 80)"
    (current-width w)]
   [("--max-blank-lines")
    n
    "max consecutive blank lines -- must be a natural number or +inf.0 (default: 1)"
    (current-max-blank-lines n)]
   [("--indent")
    n
    "indentation level for subsequent lines -- must be a natural number (default: 0)"
    (current-indent n)]
   [("--config")
    conf
    "configuration file -- must be a path or - (.fmt.rkt if exists or else standard config) or -standard (standard config)"
    (current-config conf)]
   [("-i")
    "Modifies the file in-place"
    (current-in-place? #t)]
   #:args args
   args))

(define the-map
  (case (current-config)
    [("-") (cond
             [(file-exists? ".fmt.rkt")
              (dynamic-require ".fmt.rkt" 'the-formatter-map (λ () empty-formatter-map))]
             [else empty-formatter-map])]
    [("-standard") empty-formatter-map]
    [else (dynamic-require (current-config) 'the-formatter-map (λ () empty-formatter-map))]))


(define-struct strip-ctx (lines reader))

(define (strip-reader lines)
  ;; find the index of the first string starting with "#reader" in the LINES
  (define READER-IDX 2)
  (define READER (list-ref lines READER-IDX))

  (define LINES* (list-set lines READER-IDX ""))

  (make-strip-ctx (string-join LINES* "\n") READER))

(define (reinsert-reader s ctx)
  (define LINES (string-split s "\n"))
  (define LINES* (list-update LINES 2
                              (lambda (l) (string-append l (strip-ctx-reader ctx)))))
  (string-join LINES* "\n"))

(define (do-format-normal s)
  (program-format s
                  #:formatter-map the-map
                  #:width (current-width)
                  #:max-blank-lines (current-max-blank-lines)))

(define (do-format-strip-reader lines)
  (define ctx (strip-reader lines))
  (define formatted (do-format-normal (strip-ctx-lines ctx)))
  (reinsert-reader formatted ctx))

(define (contains-reader? lines)
  (and
   (>= (length lines) 3)
   (string-contains? (list-ref lines 2) "#reader")))

(define (do-format s)
  (define LINES (string-split s "\n"))
  (if (contains-reader? LINES)
      (do-format-strip-reader LINES)
      (do-format-normal s)))

(match filenames
  ['() (displayln (do-format (string-join (for/list ([line (in-lines)]) line) "\n")))]
  [_
   (for ([filename (in-list filenames)])
     (define out (do-format (file->string filename)))
     (case (current-in-place?)
       [(#f) (displayln out)]
       [(#t) (with-output-to-file filename
               #:exists 'must-truncate
               (λ () (displayln out)))]))])
