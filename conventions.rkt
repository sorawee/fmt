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
   #;(define
       (a b)
       c
       d
       e)
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
   #;(define a b)
   (match xs
     [(list -define (? nl?) ... -head (? nl?) ... -body)
      #:when (not (ormap require-newline? (list -define -head -body)))
      (flat (hs-concat (map pretty (list -define -head -body))))]
     [_ fail])

   ;; regular case
   #;(define (a b)
       c
       d
       e)
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
       #;[]
       ['() (pretty clause)]
       #;[x]
       [(list _pattern) (pretty clause)]
       #;[x y]
       [(list -pattern (? nl?) ... -something)
        (pretty (node comment opener closer (list -pattern -something)))]
       #;[x
          #:when a b
          c]
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

       #;[a
          b
          c]
       [(list -pattern (? nl?) ... -body ..2)
        (pretty-node clause
                     (v-append (pretty -pattern)
                               (v-concat (map pretty -body))))])]
    [_ (pretty clause)]))

(define ((hook-match pretty) xs)
  (match xs
    #;(match)
    [(list -match)
     (if (require-newline? -match)
         (v-append (pretty -match)
                   (text "   "))
         (pretty -match))]
    #;(match x)
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
    #;(match x
        [a b])
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

(define ((hook-cond-clause pretty) clause)
  (match clause
    [(node comment opener closer xs)
     (match xs
       ['() (pretty clause)]
       [(list _) (pretty clause)]
       [(list -conditional (? nl?) ... -stuff)
        (pretty (node comment opener closer (list -conditional -stuff)))]
       [(list -conditional (? nl?) ... -stuff ..2)
        (pretty-node clause
                     (v-append (pretty -conditional)
                               (flush-if (require-newline? (last -stuff))
                                         (v-concat (map pretty -stuff)))))])]
    [_ (pretty clause)]))

(define ((hook-cond pretty) xs)
  (match xs
    #;(cond)
    [(list -cond)
     (if (require-newline? -cond)
         (v-append (pretty -cond)
                   space)
         (pretty -cond))]
    #;(cond
        [a b])
    [(list -cond -clause ...)
     (v-append
      (pretty -cond)
      (h-append
       space
       (flush-if (require-newline? (last -clause))
                 (v-concat (map (hook-cond-clause pretty) -clause)))))]))


(define ((hook-let-bindings pretty) bindings)
  (pretty bindings))

(define ((hook-let pretty) xs)
  (alt
   ;; fallback: v-append every element
   (match xs
     #;(let)
     [(list -let)
      (if (require-newline? -let)
          (v-append (pretty -let)
                    (text "   "))
          (pretty -let))]
     #;(let ())
     [(list -let (? nl?) ... -bindings)
      (define bindings* ((hook-let-bindings pretty) -bindings))
      (cond
        [(require-newline? -bindings)
         (v-append (pretty -let)
                   (h-append (text "   ") bindings*)
                   space)]
        [else (v-append (pretty -let)
                        (h-append (text "   ") bindings*))])]
     #;(let ()
         a)
     [(list -let (? nl?) ... -bindings (? nl?) ... -body ..1)
      (define bindings* ((hook-let-bindings pretty) -bindings))
      (v-append (pretty -let)
                (h-append (text "   ") bindings*)
                (h-append space (flush-if (require-newline? (last -body))
                                          (v-concat (map pretty -body)))))])
   ;; actual pretty one
   (match xs
     ;; fallback already deals with this
     [(cons -let _)
      #:when (require-newline? -let)
      fail]
     #;(let) ; fallback already deals with this
     [(list _) fail]
     #;(let ())
     [(list -let (? nl?) ... -bindings)
      (define bindings* ((hook-let-bindings pretty) -bindings))
      (cond
        [(require-newline? -bindings)
         (v-append (hs-append (pretty -let) bindings*)
                   space)]
        [else (hs-append (pretty -let) bindings*)])]
     #;(let ()
         a)
     [(list -let (? nl?) ... (and -bindings (node _ _ _ _)) (? nl?) ... -body ..1)
      (v-append
       (h-append (pretty -let) space ((hook-let-bindings pretty) -bindings))
       (h-append space (flush-if (require-newline? (last -body))
                                 (v-concat (map pretty -body)))))]
     #;(let a ()
         b)
     [(list -let (? nl?) ...
            (and -name (atom _ _ _))
            (? nl?) ...
            -bindings
            -body ..1)
      (cond
        [(require-newline? -name) fail]
        [else
         (v-append
          (hs-append (pretty -let) (pretty -name) ((hook-let-bindings pretty) -bindings))
          (h-append space (flush-if (require-newline? (last -body))
                                    (v-concat (map pretty -body)))))])]
     [_ fail])))

(define (hook-standard name)
  (case name
    [("define") hook-define]
    [("match") hook-match]
    [("cond") hook-cond]
    [("let") hook-let]
    [else #f]))
