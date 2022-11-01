#lang racket/base

(provide formatter
         form-map/c
         compose-form-map
         standard-form-map
         module-form-map
         :form-map)

(require syntax/parse/define
         pprint-compact
         "../private/params.rkt"
         "racket/private/params.rkt"
         "racket/private/realign.rkt"
         "racket/private/read.rkt"
         "racket/private/core.rkt"
         "racket/private/form-map.rkt"
         "racket/private/conventions.rkt")

(define (formatter content #:source-path source-path)
  (define form-map (or (:form-map) standard-form-map))
  (define width (:width))
  (define indent (:indent))
  (define max-blank-lines (:max-blank-lines))
  (define doc (realign (read-all content source-path max-blank-lines)))
  (pretty-format #:width width #:indent indent (pretty-doc doc form-map)))

(define-syntax-parse-rule (module-form-map x:id)
  (dynamic-require '(submod x form-map) 'form-map))

(module+ form-map
  (provide (rename-out [standard-form-map form-map])))
