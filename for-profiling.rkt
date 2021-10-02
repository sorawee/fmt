#lang racket

(module+ main
  (require fmt)
  (void (program-format (file->string "tests/large.rkt"))))
