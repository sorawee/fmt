#lang racket

(abc ; def
)

(; def
 abc)

(abc
 def ; ghi
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


'(abc ; def
 )

'(; def
 abc)

'(abc
 def ; ghi
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#;(abc ; def
  )

#;(; def
  abc)

#;(abc
  def ; ghi
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#; ; abc
def

' ; abc
def

#; 'abc

'#;abc
def

#;#;#;
a
b
c



' ; abc
' ; def
g


(let loop)


(foo 'abc

     def)

(match #;(x) ; def
  y
  [z 1])

#;(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)
#;(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)

(for/list ([x 10]) #:break (even? x) 1)

(syntax-parse stx #:disable-colon-notation #:context 1 #:literals #;123 2 #:track-literals
              [hello #:with a 3 #:when bbb #:fail-when blah "def" world])


(for ([a (in-list xs)]
      #:unless
      ;; a comment is here
      (equal? a 1))
  a)

(for ([a (in-list xs)]
      #:unless
      #;(a comment is here)
      (equal? a 1))
  a)

(syntax-parse stx
  [b
   #:with b #:with a 1 #:with #:with z 3
   2])
