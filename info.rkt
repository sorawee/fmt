#lang info
(define collection "fmt")
(define deps '("pretty-expressive"
               "syntax-color-lib"
               "base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/fmt.scrbl" ())))
(define pkg-desc "an extensible code formatter for Racket")
(define version "0.0.2")
(define pkg-authors '(sorawee))
(define license '(Apache-2.0 OR MIT))
(define compile-omit-files '("tests/test-cases" "tests/benchmarks" "scribblings/examples" "experiments"))
(define test-omit-paths '("tests/test-cases" "tests/benchmarks" "scribblings/examples" "experiments"))
(define raco-commands
  '(("fmt"
     fmt/raco
     "format a Racket code"
     42)))
