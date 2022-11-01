#lang racket/base

(require "private/conventions.rkt")

(define (form-map s)
  (case s
    [("if") (standard-form-map "if")]
    [("provide" "require") (standard-form-map "require")]

    [("define") (standard-form-map "define")]
    [("define-for-syntax" "define-values") (standard-form-map "define-for-syntax")]
    [("define-syntax-rule") (standard-form-map "define-for-syntax")]
    [("define-syntax" "define-syntaxes" "define-values-for-syntax") (standard-form-map "define-for-syntax")]

    [("λ" "lambda") (standard-form-map "define-for-syntax")]

    [("let*") (standard-form-map "let*")]
    [("let-values" "let*-values" "letrec" "letrec-values") (standard-form-map "parameterize")]
    [("let-syntax" "letrec-syntax" "let-syntaxes" "letrec-syntaxes") (standard-form-map "parameterize")]
    [("with-syntax" "with-syntax*" "with-handlers" "with-handlers*") (standard-form-map "parameterize")]
    [("parameterize" "parameterize*") (standard-form-map "parameterize")]

    [("letrec-syntaxes+values") (standard-form-map "letrec-syntaxes+values")]

    [("begin" "begin-for-syntax") (standard-form-map "begin")]
    [("begin0" "module+") (standard-form-map "begin0")]
    [("module" "module*") (standard-form-map "module")]

    [("cond" "case-lambda") (standard-form-map "cond")]

    [("syntax-rules" "case") (standard-form-map "syntax-rules")]

    [("syntax-case") (standard-form-map "syntax-case")]

    [("syntax/loc" "quasisyntax/loc") (standard-form-map "syntax/loc")]
    [("when" "unless") (standard-form-map "when")]

    [("for/fold" "for*/fold") (standard-form-map "for/fold")]

    [("for" "for*") (standard-form-map "for")]
    [("for/list" "for*/list") (standard-form-map "for")]
    [("for/and" "for*/and" "for/or" "for*/or") (standard-form-map "for")]
    [("for/first" "for*/first" "for/last" "for*/last") (standard-form-map "for")]
    [("for/hash" "for*/hash") (standard-form-map "for")]
    [("for/hasheq" "for*/hasheq") (standard-form-map "for")]
    [("for/hasheqv" "for*/hasheqv") (standard-form-map "for")]
    [("for/vector" "for*/vector") (standard-form-map "for")]

    [("let") (standard-form-map "let")]

    [("struct") (standard-form-map "struct")]
    [("define-struct") (standard-form-map "define-struct")]

    [else (standard-form-map #f)]))

(module+ form-map
  (provide form-map))
