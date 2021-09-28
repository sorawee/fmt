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

This package provides a tool @exec{raco fmt} to reformat a Racket code.

The package uses @racketmodname[pprint-compact], a very expressive pretty printer library, to compute the most optimal layout of the output code, and use @racketmodname[syntax-color/module-lexer #:indirect] to lex the input program.

The interface to allow users to extend formatting style is extremely unstable and is still a work in progress. For now, the only thing that is stable is the command @exec{raco fmt}.

@section{Running @exec{raco fmt}}

The @exec{raco fmt} command accepts the following flags:

@itemlist[
  @item{@DFlag{width} @nonterm{width} --- set the page width limit to @nonterm{width},
        which must be either a natural number or @racket[+inf.0].
        The default value is @racket[80].}
  @item{@DFlag{max-blank-lines} @nonterm{n} --- set the maximum consecutive blank lines limit
        to @nonterm{n}, which must be either a natural number of @racket[+inf.0].
        The default value is @racket[1].}
  @item{@DFlag{out} @nonterm{path} --- output to @nonterm{path},
        which must be either a path or @tt{-} (output to standard output)
        or @tt{-self} (rewrite the input file).
        The default value is @tt{-}.}
]

@section{Examples}

Given the file @filepath{example.rkt} shown on the left, running @exec{raco fmt --width 40 example.rkt} would output the program on the right:

@compare[
  @external-file["examples/example.rkt" #:name "example.rkt"]
  @external-file/format["examples/example.rkt" #:name "formatted example.rkt"]
]

@section{Unstable concepts}

A @deftech{formatter} is a function that accepts a code fragment that returns a @racket[doc?].
In principle, you can create your own @tech{formatter}, but you need to understand many structures
that are currently undocumented and unstable.

@section{Unstable API}

@defproc[(program-format [s string?]
                         [formatter-map (-> string? #,(tech "formatter"))]
                         [#:width width (or/c natural-number/c +inf.0) 80]
                         [#:max-blank-lines max-blank-lines (or/c natural-number/c +inf.0) 1])
         string?]{
  Formats string @racket[s] with @racket[formatter-map] under the page width limit @racket[width]
  and the maximum consecutive blank lines limit @racket[max-blank-lines].

  @examples[#:eval evaluator
    (display
     (program-format "(define (foo) bar baz) (define-like (foo) bar baz)"
                     standard-formatter-map))
    (define (my-formatter-map s)
      (cond
        [(and (string? s) (string-prefix? s "define-"))
         (standard-formatter-map "define")]
        [else (standard-formatter-map s)]))
    (display
     (program-format "(define (foo) bar baz) (define-like (foo) bar baz)"
                     my-formatter-map))
  ]
}


@defproc[(standard-formatter-map [name (or/c string? #f)]) #,(tech "formatter")]{
  Maps many common @racket[name]s to various formatter. When @racket[name] is @racket[#f],
  the function returns the @tech{formatter} for function application.
}



@section{Related work}

@itemlist[
  @item{@link["https://github.com/racket/drracket/"]{DrRacket} is a Racket editor. It has an indenter which can re-indent code, but cannot in general re-format code. Users can add custom keywords to the four predefined keyword categories, but cannot define a new category (without a plug-in).}
  @item{@link["https://github.com/mxork/raco-format/"]{@tt{raco-format}} is a command-line tool that invokes DrRacket's indenter.}
  @item{@link["https://racket-mode.com/"]{Racket Mode} is a mode in Emacs for editing Racket code.
        Similar to DrRacket, it has an indenter. Compared to DrRacket, Racket Mode is more customizable on one axis
        (more keyword categories) but less customizable on another axis (must map each keyword one-by-one)}
  @item{@racketmodname[racket/pretty] is a library for pretty printing an S-expression value.
        It does not support comments, is less expressive and less optimal than @racketmodname[fmt].}
  @item{@racketmodname[pprint #:indirect] is a library for pretty printing an arbitrary document.
       It is based on Wadler's pretty printer, which is less expressive and less optimal than @racketmodname[pprint-compact]
       (but has better performance).}
  @item{@link["https://github.com/Shuumatsu/racket-pretty-printer/"]{@tt{racket-pretty-printer}} is a Racket formatter written in Haskell.
        It uses Wadler's pretty printer, so it has the limitations as described in the above item.}
]
