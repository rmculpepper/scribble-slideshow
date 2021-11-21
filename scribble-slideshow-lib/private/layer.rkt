;; Copyright 2021 Ryan Culpepper.
;; Licensed under the Apache 2.0 license. See LICENSE.

#lang racket/base
(require racket/match
         racket/class
         pict
         ppict/pict
         ppict/align
         ppict/zone
         "style.rkt")
(provide (all-defined-out))

;; Note: this module does not depend on slide.rkt or slideshow. See slide.rkt
;; for zones and layers that depend on slideshow config and slide properties.

;; Downside: we can't ask a layer for its width (or height) without a
;; slide-config. This is okay (maybe?), because we don't *need* the width to
;; update the width until we do the scribble->picts conversion, but it means
;; that all width-dependent things must be delayed!

;; ============================================================
;; Layers

(define layer<%>
  (interface ()
    get-z        ;; -> ExtendedReal
    update-style ;; StyleHash -> StyleHash

    ;; type LayerPre

    ;; Used to determine placement of current content based on this layer's
    ;; content on all related slides (eg next, alts).

    update-pre   ;; LayerPre/#f Pict -> LayerPre
    max-pre      ;; LayerPre LayerPre -> LayerPre

    place        ;; (Listof Pict) LayerPre Pict -> Pict
    ;; Places the contents onto the given base, where base is a full-page pict.
    ))

(define (layer? v) (is-a? v layer<%>))

(define (layer<? a b)
  (< (send a get-z) (send b get-z)))

;; ----------------------------------------

(define layer-base%
  (class* object% (layer<%>)
    (init-field [style (hasheq)]    ;; StyleHash, should set 'block-width
                [z (next-auto-z)])  ;; Real
    (super-new)

    ;; FIXME: change style field to upstyle (SPStyle -> SPStyle) ??

    (define/public (get-z) z)

    (define/public (update-style istyle)
      (for/fold ([istyle istyle])
                ([(k v) (in-hash style)])
        (hash-set istyle k v)))

    (abstract update-pre)
    (abstract max-pre)

    ;; ----
    (abstract place)
    ))

(define h-layer-base%
  (class layer-base%
    (init-field gap)
    (super-new)

    ;; type LayerPre = Real  -- height of all picts so far

    ;; Placement of current content is determined by maximum height of content
    ;; in this layer on all related slides. For example: coord, grid, etc.

    (define/public (get-gap) gap)

    (define/override (update-pre lpre p)
      (if lpre
          (+ lpre (get-gap) (pict-height p))
          (pict-height p)))
    (define/override (max-pre lpre1 lpre2)
      (max lpre1 lpre2))
    ))

(define ps-layer-base%
  (class layer-base%
    (super-new)

    ;; type LayerPre = (Listof BlankPict)

    ;; Placement of current content is determined by maximum number of picts
    ;; (and maximum dimensions per pict index) in this layer on all related
    ;; slides. For example: cascade, tile.

    (define/override (update-pre lpre p)
      (append (or lpre null) (list (blank (pict-width p) (pict-height p)))))
    (define/override (max-pre ps1 ps2)
      (match* [ps1 ps2]
        [[ps1 '()] ps1]
        [['() ps2] ps2]
        [[(cons p1 ps1) (cons p2 ps2)]
         (cons (blank (max (pict-width p1) (pict-width p2))
                        (max (pict-height p1) (pict-height p2)))
               (max-pre ps1 ps2))]))
    ))

(define layer%
  (class h-layer-base%
    (init-field placer  ;; RefpointPlacer
                zone)   ;; Zone, used to update style with width?
    (inherit get-gap)
    (super-new)

    (define zplacer (subplacer placer zone))
    (define halign
      (or (send placer check-associative-vcompose)
          (error 'layer "placer has incompatible compose function: ~e" placer)))

    ;; place : (Listof Pict) LayerPre Pict -> Pict
    (define/override (place ps lpre base)
      (define p (combine-picts ps lpre))
      (send zplacer place base (list p)))

    ;; combine-picts : (Listof Pict) LayerPre -> Pict
    (define/public (combine-picts ps lpre)
      (define-values (p _newsep) (send placer compose-elements ps))
      (inset-to/align p #f lpre (make-align halign 't)))
    ))

;; ----------------------------------------

(define (layer placer zone
               #:z [z (next-auto-z)]
               #:gap [gap 24]
               #:style [style (hasheq)])
  (new layer% (z z) (gap gap) (style style) (placer placer) (zone zone)))

;; FIXME: add [#:block-width rel/abs], set in update-style?

(define (next-auto-z)
  (set! auto-z (+ auto-z auto-dz))
  auto-z)

(define auto-z 1.0)
(define auto-dz 0.000001)
