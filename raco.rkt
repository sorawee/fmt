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
   #:args (filename)
   filename))

(display (program-format (file->string filename)
                         standard-formatter-map
                         #:width (current-width)
                         #:max-blank-lines (current-max-blank-lines)))
