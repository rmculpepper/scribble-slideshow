;; Copyright 2020 Ryan Culpepper.
;; Licensed under the Apache 2.0 license. See LICENSE.

#lang racket/base
(require "pict.rkt"
         "private/slide.rkt")
(provide (all-from-out "pict.rkt")
         scribble-slides
         scribble-slides*
         part/make-slides
         in-style
         in-layer
         make-layer)

;; ----------------------------------------
;; Language

(module* lang racket/base
  (require (only-in scribble/doclang [#%module-begin scribble-module-begin])
           (submod ".."))
  (provide (except-out (all-from-out racket/base) #%module-begin)
           (rename-out [module-begin #%module-begin])
           (all-from-out (submod "..")))

  (define-syntax-rule (module-begin id . body)
    (scribble-module-begin
     id post-process ()
     (module* main racket/base ;; module+ does not seem to work ...
       (require scribble-slideshow
                (only-in (submod "..") [id id]))
       (scribble-slides* id))
     . body))

  (define (post-process doc) doc))

;; ----------------------------------------
;; Reader

;; Adapted from scribble/manual/lang/reader.rkt
(module reader scribble/base/reader
  (submod scribble-slideshow lang)
  #:wrapper1 (lambda (t) (cons 'doc (t))))
