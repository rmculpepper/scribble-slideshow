;; Copyright 2020 Ryan Culpepper.
;; Licensed under the Apache 2.0 license. See LICENSE.

#lang racket/base
(require "private/style.rkt"
         "private/content.rkt"
         "private/block.rkt"
         "private/slide.rkt")
(provide flow-pict
         current-sp-style ;; FIXME, remove, use style-transformer on title instead?
         text-post-property
         elem-post-property
         style-transformer

         scribble-slide-picts
         part/make-slides
         in-style
         in-layer
         layer
         slide-layer
         slide-zone)
