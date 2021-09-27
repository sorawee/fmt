#lang racket/base

; don't run this file for testing:
(module test racket/base)

(require racket/cmdline
         racket/file
         "core.rkt"
         "conventions.rkt")

(define filename
  (command-line
   #:args (filename)
   filename))

(display (program-format (file->string filename)
                         standard-format))
