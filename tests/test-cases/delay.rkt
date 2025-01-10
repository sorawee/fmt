#lang racket

(delay/thread (begin0 (run-in-other-place p* error?)
                (when (zero? (modulo i 10))
                  (eprintf "."))))

(delay/thread #:wait-for wait-evt-expr #:work-while while-evt-expr #:tick tick-secs-expr #:use use-ratio-expr
 (begin0 (run-in-other-place p* error?)
   (when (zero? (modulo i 10))
     (eprintf "."))))
