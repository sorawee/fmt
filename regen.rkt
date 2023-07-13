;; This file is for regenerating test files

#lang racket

(require file/glob)

(define paths '("tests/test-cases/*.rkt"
                "tests/benchmarks/*.rkt"))

(define out-ext "out")

(define (regen ext)
  (for ([f (flatten (map glob paths))])
    (printf "formatting ~a\n" f)
    (system (format "raco fmt ~a > ~a.~a" f f ext))))

(module+ main
  (regen out-ext))

(module+ test
  (require rackunit)

  (define-check (check-file f)
    (unless (equal? (file->string (format "~a.~a" f check-ext))
                    (file->string (format "~a.~a" f out-ext)))
      (fail-check (format "Found a mismatch on '~a'" f))))

  (define check-ext "out-check")
  (regen check-ext)
  (for ([f (flatten (map glob paths))])
    (check-file f)
    (delete-file (format "~a.~a" f check-ext))))
