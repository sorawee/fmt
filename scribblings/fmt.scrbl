#lang scribble/manual
@require["util.rkt"
         racket/runtime-path
         racket/file
         @for-label[fmt
                    racket/base]]

@(define-runtime-path fmt-file "examples/test-formatted.rkt")

@title{fmt: an extensible code formatter for Racket}
@author[@author+email["Sorawee Porncharoenwase" "sorawee.pwase@gmail.com"]]

@defmodule[fmt]

This package provides a tool @exec{raco fmt} to reformat a Racket code, and an API to invoke the tool programatically.

This library part, however, is still a work in progress. Its interface is extremely unstable. So for now, the only thing that is stable is the command @exec{raco fmt}.

@section{Running @exec{raco fmt}}

By running @exec{raco fmt test.rkt > test-formatted.rkt}

@external-file["examples/test.rkt"]

would be formatted as:

@do-format["examples/test.rkt" "examples/test-formatted.rkt"]

@delete-directory/files[fmt-file #:must-exist? #f]
