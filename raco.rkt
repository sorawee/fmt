#lang racket/base

; don't run this file for testing:
(module test racket/base)

(require racket/cmdline
         racket/file
         "core.rkt"
         "conventions.rkt")

(define current-width
  (make-parameter
   80
   (λ (n)
     (define as-num (string->number n))
     (cond
       [(and as-num (or (exact-nonnegative-integer? as-num) (= as-num +inf.0)))
        as-num]
       [else (raise-user-error
              'width
              "must be either a natural number of +inf.0, given: ~a"
              n)]))))

(define current-max-blank-lines
  (make-parameter
   1
   (λ (n)
     (define as-num (string->number n))
     (cond
       [(and as-num (or (exact-nonnegative-integer? as-num) (= as-num +inf.0)))
        as-num]
       [else (raise-user-error
              'max-blank-lines
              "must be either a natural number of +inf.0, given: ~a"
              n)]))))

(define current-output (make-parameter "-"))
(define current-config (make-parameter "-"))

(define filename
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
   [("--config")
    conf
    "configuration file -- must be a path or - (.fmt.rkt if exists or else standard config) or -standard (standard config)"
    (current-config conf)]
   [("--out")
    o
    "output path -- must be a path or - (stdout) or -self (input file) (default: -)"
    (current-output o)]
   #:args (filename)
   filename))

(define the-map
  (case (current-config)
    [("-") (cond
             [(file-exists? ".fmt.rkt")
              (dynamic-require ".fmt.rkt" 'the-formatter-map)]
             [else standard-formatter-map])]
    [("-standard") standard-formatter-map]
    [else (dynamic-require (current-config) 'the-formatter-map)]))

(define s
  (program-format (file->string filename)
                  the-map
                  #:width (current-width)
                  #:max-blank-lines (current-max-blank-lines)))

(define (write-file path)
  (with-output-to-file path
    #:exists 'replace
    (λ () (displayln s))))

(case (current-output)
  [("-") (displayln s)]
  [("-self") (write-file filename)]
  [else (write-file (current-output))])
