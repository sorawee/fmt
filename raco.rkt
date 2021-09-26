#lang racket

(require racket/cmdline
         "core.rkt"
         "conventions.rkt")

(define filename
  (command-line
   #:args (filename)
   filename))

(display (program-format (file->string filename)
                         standard-format))
