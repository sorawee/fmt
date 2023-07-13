#lang racket

(module+ test
  (require racket/runtime-path
           raco/all-tools
           rackunit)

  (define-runtime-path test-hash-bang.rkt "tests/test-hash-bang.rkt")
  (check member 'execute (file-or-directory-permissions test-hash-bang.rkt)
         "before -i")
  (define raco-fmt (hash-ref (all-tools) "fmt"))
  (parameterize ([current-command-line-arguments (vector "-i" (~a test-hash-bang.rkt))])
    (dynamic-require (second raco-fmt) #f))
  (check member 'execute (file-or-directory-permissions test-hash-bang.rkt)
         "after -i"))
