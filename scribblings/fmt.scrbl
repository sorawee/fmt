#lang scribble/manual
@require[scribble/bnf
         "util.rkt"
         @for-label[fmt]
         @for-syntax[racket/base]]

@title{fmt: an extensible code formatter for Racket}
@author[@author+email["Sorawee Porncharoenwase" "sorawee.pwase@gmail.com"]]

@defmodule[fmt]

This package provides a tool @exec{raco fmt} to reformat a Racket code, and an API to invoke the tool programatically.

This library part, however, is still a work in progress. Its interface is extremely unstable. So for now, the only thing that is stable is the command @exec{raco fmt}.

@section{Running @exec{raco fmt}}

Given the file @filepath{example.rkt} shown on the left, running @exec{raco fmt --width 50 test.rkt} would output the program on the right:

@compare[
  @external-file["examples/example.rkt" #:name "example.rkt"]
  @external-file/format["examples/example.rkt" #:name "formatted example.rkt"]
]

The @exec{raco fmt} command accepts the following flags:

@itemlist[
  @item{@DFlag{width} @nonterm{width} --- set the page width limit to @nonterm{width},
        which must be either a natural number or @racket[+inf.0].
        The default value is @racket[80].}
  @item{@DFlag{max-blank-lines} @nonterm{n} --- set the max consecutive blank lines limit
        to @nonterm{n}, which must be either a natural number of @racket[+inf.0].
        The default value is @racket[1].}
]
