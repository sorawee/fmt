#lang racket/base

(provide hook-standard
         hook-define)
(require racket/match
         racket/list
         pprint-compact
         "core.rkt")

(define ((hook-define-head pretty) xs)
  (pretty xs))

(define ((hook-define pretty) xs)
  (define req-last-newline? (require-newline? (last xs)))

  (alt
   ;; fallback case
   (match xs
     [(list -define)
      (if (require-newline? -define)
          (v-append (pretty -define)
                    space)
          (pretty -define))]
     [(list -define (? nl?) ... -head)
      (v-append
       (pretty -define)
       (h-append space (flush-if req-last-newline?
                                 ((hook-define-head pretty) -head))))]
     [(list -define (? nl?) ... -head (? nl?) ... -xs ..1)
      (v-append
       (pretty -define)
       (h-append space (v-append
                        ((hook-define-head pretty) -head)
                        (flush-if req-last-newline?
                                  (v-concat (map pretty -xs))))))])

   ;; fit in one line case -- no need for hook-define-head since it's flat
   (match xs
     [(list -define (? nl?) ... -head (? nl?) ... -body)
      #:when (not (ormap require-newline? (list -define -head -body)))
      (flat (hs-concat (map pretty (list -define -head -body))))]
     [_ fail])

   ;; regular case
   (match xs
     [(list -define (? nl?) ... -head (? nl?) ... -body ..1)
      #:when (not (require-newline? -define))
      (v-append
       (h-append (pretty -define)
                 space
                 ((hook-define-head pretty) -head))
       (h-append space
                 (flush-if req-last-newline? (v-concat (map pretty -body)))))]
     [_ fail])))

(define ((hook-match-clause pretty) clause)
  (match clause
    [(node comment opener closer xs)
     (match xs
       ['() (pretty clause)]
       [(list _pattern) (pretty clause)]
       [(list -pattern (? nl?) ... -something)
        (pretty (node comment opener closer (list -pattern -something)))]
       [(list -pattern
              (? nl?)
              ...
              (and -when (atom _ "#:when" 'hash-colon-keyword))
              (? nl?)
              ...
              -e)
        (pretty-node clause
                     (v-append (pretty -pattern)
                               (flush-if
                                (require-newline? -e)
                                (alt
                                 (if (require-newline? -when)
                                     fail
                                     (hs-append (pretty -when) (pretty -e)))
                                 (v-append (pretty -when) (pretty -e))))))]
       [(list -pattern
              (? nl?)
              ...
              (and -when (atom _ "#:when" 'hash-colon-keyword))
              (? nl?)
              ...
              -e
              -body
              ..1)
        (pretty-node clause
                     (v-append (pretty -pattern)
                               (alt
                                (if (require-newline? -when)
                                    fail
                                    (hs-append (pretty -when) (pretty -e)))
                                (v-append (pretty -when) (pretty -e)))
                               (v-concat (map pretty -body))))]

       [(list -pattern (? nl?) ... -body ..2)
        (pretty-node clause
                     (v-append (pretty -pattern)
                               (v-concat (map pretty -body))))])]
    [_ (pretty clause)]))

(define ((hook-match pretty) xs)
  (match xs
     [(list -match)
      (if (require-newline? -match)
          (v-append (pretty -match)
                    (text "   "))
          (pretty -match))]
     [(list -match (? nl?) ... -x)
      (cond
        [(and (require-newline? -match) (require-newline? -x))
         (v-append (pretty -match)
                   (h-append (text "   ") (pretty -x))
                   (h-append space))]
        [(require-newline? -match)
         (v-append (pretty -match)
                   (h-append (text "   ") (pretty -x)))]
        [(require-newline? -x)
         (v-append (h-append (pretty -match) space (pretty -x))
                   space)]
        [else (h-append (pretty -match) space (pretty -x))])]
     [(list -match (? nl?) ... -x (? nl?) ... -clause ..1)
      (define clauses*
        (flush-if (require-newline? (last -clause))
                  (v-concat (map (hook-match-clause pretty) -clause))))
      (if (require-newline? -match)
          (v-append (pretty -match)
                    (h-append (text "   ") (pretty -x))
                    (h-append space clauses*))
          (v-append (h-append (pretty -match) space (pretty -x))
                    (h-append space clauses*)))]))

(define (hook-standard name)
  (case name
    [("define") hook-define]
    [("match") hook-match]
    [else #f]))
