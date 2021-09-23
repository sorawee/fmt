#lang racket/base

(provide hook-standard
         hook-define)
(require racket/match
         racket/list
         pprint-compact
         "core.rkt")

(define (hook-define xs pretty)
  (define xs* (map pretty xs))
  (define req-last-newline? (require-newline? (last xs)))

  (apply
   alt
   (append
    ;; fallback case
    (if req-last-newline?
        (list (v-append (first xs*)
                        (h-append space (v-concat (rest xs*)))))
        (list (v-append (first xs*)
                        (h-append space (flush (v-concat (rest xs*)))))))
    ;; fit in one line case
    (match xs
      [(list _ _ _)
       #:when (not (ormap require-newline? xs))
       (list (flat (hs-concat xs*)))]
      [_ '()])

    ;; regular case
    (match xs
      [(list head _ _ _ ...)
       #:when (not (require-newline? head))
       (if req-last-newline?
           (list (v-append
                  (h-append (first xs*)
                            space
                            (second xs*))
                  (h-append space (flush (v-concat (rest (rest xs*)))))))
           (list (v-append
                  (h-append (first xs*)
                            space
                            (second xs*))
                  (h-append space (v-concat (rest (rest xs*)))))))]
      [_ '()]))))

(define (hook-standard name)
  (case name
    [("define") hook-define]
    [else #f]))
