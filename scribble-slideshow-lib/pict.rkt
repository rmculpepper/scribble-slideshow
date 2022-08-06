;; Copyright 2020 Ryan Culpepper.
;; Licensed under the Apache 2.0 license. See LICENSE.

#lang racket/base
(require racket/contract/base
         (prefix-in s: scribble/core)
         (prefix-in s: scribble/decode)
         (only-in pict pict?)
         "private/style.rkt"
         "private/content.rkt"
         "private/block.rkt"
         "private/slide.rkt")

(provide (contract-out
          [flow-pict
           (->* [] [#:style (or/c #f s:style? symbol? string?) #:resolve? boolean?]
                #:rest (listof s:pre-flow?)
                pict?)]
          [current-sp-style  ;; deprecated
           (parameter/c (and/c hash? hash-eq? immutable?))]

          [compound*
           (->* [] [#:style (or/c #f s:style? symbol? string?) #:layer (or/c #f layer?)]
                #:rest (listof s:pre-flow?)
                s:block?)]
          [part/make-slides
           (-> (-> any) s:part?)])

         text-post-property
         elem-post-property
         style-transformer
         style-diffs

         scribble-slide-picts

         layer
         slide-layer
         slide-zone)
