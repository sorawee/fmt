#lang racket

(require rackunit)

(test-begin (check-equal? 1 2) (check-equal? 3 4))
(test-case "hello" (check-equal? 1 2) (check-equal? 3 4))
(test-suite "hello" (check-equal? 1 2) (check-equal? 3 4))
