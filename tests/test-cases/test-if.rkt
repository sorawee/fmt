#lang racket

(define if1
  (if #t
      1
      0))

(define if2
  (if #t (* 2 3) (+ 3 4)))

(define if3
  (if (> (+ 2 3) (* 3 4)) #t #f))

(define (if4 subs expr restore loop shift)
    (if subs (list (restore expr (loop subs))) (list (shift expr))))

(define (if5 s loop pi) 
    (if (< s 0) (loop (+ s (* 2 pi))) s))

(define (if6 mred-launcher)
    (if (boolean? mred-launcher) (if mred-launcher 'mred 'mzscheme) #t))

(define if7
  (if ; condition
      (< 10 20)
      ; true branch
      (* 4 10)
      ; false branch
      (+ 2 4)))
