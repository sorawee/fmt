(define/contract x integer? 1)

(define/contract (f x) (-> integer? integer?)
  (cond
    [(zero? x) 1]
    [else 2]))

(define/contract (f wwwwwwwwwwwwwwww xxxxxxxxxxxxxx yyyyyyyyyyyy zzzzzzzzzzzzzzzzzz) (-> integer? integer? integer?)
  (cond
    [(zero? x) 1]
    [else 2]))
