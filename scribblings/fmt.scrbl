#lang scribble/manual
@require[scribble/bnf
         scribble/example
         "util.rkt"
         @for-label[racket/base
                    racket/string
                    racket/contract
                    fmt
                    pprint-compact]
         @for-syntax[racket/base]]

@(define evaluator (make-base-eval))
@(evaluator '(require racket/string
                      fmt))

@title{fmt: an extensible code formatter for Racket}
@author[@author+email["Sorawee Porncharoenwase" "sorawee.pwase@gmail.com"]]

@defmodule[fmt]

This package provides a tool @exec{raco fmt} to reformat Racket code.

The package uses @racketmodname[pprint-compact], a very expressive pretty printer library, to compute the most optimal layout of the output code, and uses @racketmodname[syntax-color/module-lexer #:indirect] to lex the input program.

The interface to allow users to extend formatting style is extremely unstable and is still a work in progress. For now, the only thing that is stable is the command @exec{raco fmt}.

@section{Running @exec{raco fmt}}

The @exec{raco fmt} command accepts the following flags:

@itemlist[
  @item{@DFlag{width} @nonterm{width} --- set the page width limit to @nonterm{width},
        which must be either a natural number or @racket[+inf.0].
        The default value is @racket[80].}
  @item{@DFlag{max-blank-lines} @nonterm{n} --- set the maximum consecutive blank lines limit
        to @nonterm{n}, which must be either a natural number or @racket[+inf.0].
        The default value is @racket[1].}
  @item{@DFlag{indent} @nonterm{n} --- set the indentation level for subsequent lines to @nonterm{n},
        which must be a natural number.
        The default value is @racket[0].}
  @item{@Flag{i} --- modify the input files in-place instead of outputting to stdout.}
]

@section{Examples}

Given the file @filepath{example.rkt} shown on the left, running @exec{raco fmt --width 40 example.rkt} would output the program on the right:

@compare[
  @external-file["examples/example.rkt" #:name "example.rkt"]
  @external-file/format["examples/example.rkt" #:name "formatted example.rkt"]
]

@section{Unstable concepts}

A @deftech{formatter} is a function that accepts a code fragment and returns a @racket[doc?].
In principle, you can create your own @tech{formatter}, but you need to understand many structures
that are currently undocumented and unstable. (If you want to implement one, perhaps take a look at
@link["https://github.com/sorawee/fmt/blob/master/conventions.rkt"]{this file}.)

A @deftech{formatter map} is a function that accepts either a string or @racket[#f],
and returns either a @tech{formatter} or @racket[#f].
Conceptually, when the input is a string @racket[_s], a @tech{formatter map} should
return a @tech{formatter} that will format a form named @racket[_s], and
When the input is @racket[#f], the @tech{formatter map} should
return a @tech{formatter} that will format function application.
An exception is that the @tech{formatter map} can also return @racket[#f],
which means the @tech{formatter map} wants to let other fallback
@tech{formatter map}s to handle formatting instead.

@defthing[formatter-map/c (-> (or/c #f string?) (or/c #f #,(tech "formatter")))]{
  Recognizes a @tech{formatter map}.
}

@section{Unstable API}

@defproc[(program-format [s string?]
                         [#:formatter-map formatter-map formatter-map/c empty-formatter-map]
                         [#:width width (or/c natural-number/c +inf.0) (current-width)]
                         [#:max-blank-lines max-blank-lines (or/c natural-number/c +inf.0) (current-max-blank-lines)]
                         [#:indent indent natural-number/c (current-indent)])
         string?]{
  Formats string @racket[s] with @racket[formatter-map] under various configurations
  (see @secref["Running_raco_fmt"] for details).

  @examples[#:eval evaluator
    (define s "(define (foo) (bar baz) food) (define-like (foo) (bar baz) food)")
    (display (program-format s))
    (define (lib-define-formatter-map s)
      (cond
        [(and (string? s) (string-prefix? s "define-"))
         (standard-formatter-map "define")]
        [else #f]))
    (define (lib-bar-formatter-map s)
      (case s
        [("bar") (standard-formatter-map "cond")]
        [else #f]))
    (display (program-format s #:formatter-map lib-define-formatter-map))
    (display (program-format s #:formatter-map lib-bar-formatter-map))
    (display (program-format s #:formatter-map (compose-formatter-map
                                                lib-define-formatter-map
                                                lib-bar-formatter-map)))
  ]
}

@defthing[empty-formatter-map formatter-map/c]{
  A @tech{formatter map} that does not handle any form.
}


@defthing[standard-formatter-map formatter-map/c]{
  The fallback @tech{formatter map}. It defines format styles for the following forms:
  @racketblock[
    'lambda 'λ 'case-lambda
    'define 'define-values
    'define-for-syntax 'define-values-for-syntax
    'define-syntax 'define-syntaxes
    'define-syntax-parameter
    'define-syntax-parse-rule 'define-simple-macro
    'define-syntax-parser
    'match-define 'match-define-values
    'let 'let-values 'let*-values 'letrec 'letrec-values
    'let-syntax 'letrec-syntax 'let-syntaxes 'letrec-syntaxes
    'letrec-syntaxes+values
    'with-syntax 'with-syntax*
    'shared
    'parameterize 'parameterize* 'syntax-parameterize
    'if 'cond 'when 'unless 'case
    'match 'match*
    'begin 'begin0 'begin-for-syntax
    'syntax-parse 'syntax-parser
    'syntax-rules 'syntax-case
    'define-syntax-rule
    'match 'match*
    'syntax/loc 'quasisyntax/loc
    'module 'module* 'module+
    'provide 'require
    'for 'for/fold 'for/list 'for/vector 'for/hash 'for/hasheq 'for/hasheqv
    'for* 'for*/fold 'for*/list 'for*/vector 'for*/hash 'for*/hasheq 'for*/hasheqv
    'struct 'define-struct
    'import 'export 'link 'rename
    'class 'instantiate 'public 'private 'override 'inherit 'field 'init
  ]
  For other forms, it uses the function application style.
}

@defproc[(compose-formatter-map [f formatter-map/c] ...) formatter-map/c]{
  Constructs a @tech{formatter map} that tries the input functions in order.
  The first function that returns a @tech{formatter} will be used.
}

@section{Parameters}

@defparam[current-width width (or/c +inf.0 natural-number/c)
          #:value 102]{
  See @secref["Running_raco_fmt"] for details.
}

@defparam[current-max-blank-lines max-blank-lines (or/c +inf.0 natural-number/c)
          #:value 1]{
  See @secref["Running_raco_fmt"] for details.
}

@defparam[current-indent indent natural-number/c
          #:value 0]{
  See @secref["Running_raco_fmt"] for details.
}

@section{Related work}

@itemlist[
  @item{@link["https://docs.racket-lang.org/drracket/"]{DrRacket} (by the Racket team) is a Racket editor. It has an indenter which can re-indent code, but cannot in general re-format code. Users can add custom keywords to the four predefined keyword categories, but cannot define a new category (without a plug-in).}
  @item{@link["https://github.com/mxork/raco-format/"]{@tt{raco-format}} (by @link["https://github.com/mxork"]{Dan Anderson}) is a command-line tool that invokes DrRacket's indenter.}
  @item{@link["https://racket-mode.com/"]{Racket Mode} (by @link["https://greghendershott.com/"]{Greg Hendershott}) is a mode in Emacs for editing Racket code.
        Similar to DrRacket, it has an indenter. Compared to DrRacket, Racket Mode is more customizable on one axis
        (more keyword categories) but less customizable on another axis (must map each keyword one-by-one)}
  @item{@racketmodname[racket/pretty] (by the Racket team) is a library for pretty printing an S-expression value.
        It does not support comments, is less expressive and less optimal than @racketmodname[fmt].}
  @item{@racketmodname[pprint #:indirect] (by @link["http://calculist.org/"]{Dave Herman} and @link["https://www.asumu.xyz/"]{Asumu Takikawa}) is a library for pretty printing an arbitrary document.
       It is based on Wadler's pretty printer, which is less expressive and less optimal than @racketmodname[pprint-compact]
       (but has better performance).}
  @item{@link["https://github.com/Shuumatsu/racket-pretty-printer/"]{@tt{racket-pretty-printer}} (by @link["https://github.com/Shuumatsu"]{为世人降下祝福}) is a Racket formatter written in Haskell.
        It uses Wadler's pretty printer, so it has the limitations as described in the above item.}
  @item{@link["https://github.com/aowens-21/racket-formatting"]{@tt{racket-formatting}} (by @link["https://aowens-21.github.io/"]{Alex Owens} and @link["https://users.cs.northwestern.edu/~syt3996/"]{Shu-Hung You}) is a Racket formatter that attaches formatting information to syntax object properties. It does not attempt to pick the most optimal layout to stay within the column limit, and does not support comments.}
  @item{@link["https://github.com/russellw/racket-format/"]{@tt{racket-format}} (by @link["https://github.com/russellw"]{Russell Wallace}) is a Racket formatter written manually. It uses a greedy algorithm to decide which layout to pick, which is not optimal. It supports line-comments and the normalization features (such as sorting the list of @racket[provide]d elements), but doesn't preserve parenthesis shape.}
]
