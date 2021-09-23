#lang info
(define collection "fmt")
(define deps '("pprint-compact"
               "syntax-color-lib"
               "base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/fmt.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(sorawee))
(define license '(Apache-2.0 OR MIT))
