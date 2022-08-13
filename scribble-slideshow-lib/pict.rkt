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
           (->* [] [#:style style-like/c #:resolve? boolean?]
                #:rest (listof s:pre-flow?)
                pict?)]
          [scribble-slide-picts
           (-> s:part? (listof pict?))]

          [compound*
           (->* [] [#:style style-like/c #:layer (or/c #f layer?)]
                #:rest (listof s:pre-flow?)
                s:block?)]
          [part/make-slides
           (-> (-> any) s:part?)]

          [current-sp-style  ;; deprecated
           (parameter/c (and/c hash? hash-eq? immutable?))]
          [style-diffs
           (-> (listof style-diff/c) any)]
          [text-post-property
           (-> (-> pict? pict?) any)]
          [elem-post-property
           (-> (-> pict? pict?) any)])

         layer
         slide-layer
         slide-zone)

(define style-like/c
  (or/c s:style? string? symbol? #f sp-style-prop?))

(define (key-value-list? v)
  (and (list? v) (even? (length v))))

(define style-diff-update/c
  (or/c (list/c 'set symbol? any/c)
        (list/c 'push symbol? any/c)
        (list/c 'toggle symbol? any/c)
        (list/c 'update symbol? any/c procedure?)
        (list/c 'setf symbol? procedure?)))

(define style-diff/c
  (or/c (cons/c 'iset key-value-list?)
        (cons/c 'nset key-value-list?)
        (cons/c 'iset* (listof style-diff-update/c))
        (cons/c 'nset* (listof style-diff-update/c))
        (cons/c 'stylemap key-value-list?)
        (list/c 'ref (or/c symbol? string?))))

(begin ;; compat
  (define (text-post-property post)
    (style-diffs `((iset* (push text-post ,post)))))
  (define (elem-post-property post)
    (style-diffs `((iset* (push elem-post ,post))))))
