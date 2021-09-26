#lang racket/base

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
