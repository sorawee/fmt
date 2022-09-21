;; blah


;; MyStruct : String [List-of String]
(define-struct MyStruct [key rest])

;; AnotherStruct : MyStruct [List-of String]
(define-struct World [mystruct tried completed])
