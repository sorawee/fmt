;; This file is for regenerating test files

#lang racket

(require file/glob
         raco/all-tools)

(define paths '(["tests/test-cases/*.rkt" ()]
                ["tests/benchmarks/*.rkt" ()]
                ["tests/config-tests/file.rkt" ("--config" "tests/config-tests/config.rkt")]))

(define out-ext "out")

(define raco-fmt (hash-ref (all-tools) "fmt"))

(define (regen ext)
  (for ([test-suite (in-list paths)])
    (match-define (list path args) test-suite)
    (for ([f (in-list (glob path))])
      (printf "formatting ~a\n" f)
      (time
       (with-output-to-file (format "~a.~a" f ext)
         #:exists 'replace
         (λ ()
           (parameterize ([current-command-line-arguments
                           (apply vector (append args (list (~a f))))]
                          [current-namespace (make-base-namespace)])
             (dynamic-require (second raco-fmt) #f))))))))

(module+ main
  (regen out-ext))

(module+ test
  (require rackunit)

  (define check-ext "out-check")
  (regen check-ext)
  (for ([test-suite (in-list paths)])
    (match-define (list path _) test-suite)
    (for ([f (in-list (glob path))])
      (with-check-info (['filename f])
        (check-equal? (file->string (format "~a.~a" f check-ext))
                      (file->string (format "~a.~a" f out-ext))))

      (delete-file (format "~a.~a" f check-ext)))))
