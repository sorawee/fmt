#lang racket

(require file/glob)

(module+ main
  (define to-self '("conventions.rkt" "core.rkt"))
  (define to-out (flatten (map glob '("tests/*.rkt"))))

  (for ([f to-self])
    (system (format "raco fmt -i ~a" (~a f))))

  (for ([f to-out])
    (system (format "raco fmt ~a > ~a.out" (~a f) (~a f)))))
