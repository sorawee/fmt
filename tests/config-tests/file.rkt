#lang racket

(define (f x) (let ([x (sub1 x)]) (cond [(zero? x) 0] [else (add1 (f x))])))

(define (g x) (let ((x (sub1 x))) (cond ((zero? x) 0) (else (add1 (g x))))))
