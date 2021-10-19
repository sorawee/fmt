#lang racket

(define book-class%
  (class object%
    (field (pages 5))
    (define/public (letters)
      (* pages 500))
    (super-new)))
