;; An entry suitable for raco profile

#lang racket

(module+ main
  (require racket/cmdline
           fmt)
  (define path
    (command-line
     #:args (path)
     path))
  (void (program-format (file->string path))))
