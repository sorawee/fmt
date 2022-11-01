#;#[[fmt (config #:require [(prefix-in r: fmt/config/racket)]
                 #:with :formatter r:formatter
                 #:with :width 102
                 #:with r::form-map
                 (r:compose-form-map
                  (λ (s)
                    (case s
                      [("define/contract")
                       (r:standard-form-map "module")]
                      [else #f]))
                  (r:module-form-map fmt/config/racket/base)))]]

(define/contract (f x) (-> number? number?) (add1 x))
