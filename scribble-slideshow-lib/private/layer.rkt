;; Copyright 2021 Ryan Culpepper.
;; Licensed under the Apache 2.0 license. See LICENSE.

#lang racket/base
(require racket/match
         racket/class
         pict
         ppict/pict
         ppict/align
         ppict/zone
         (only-in ppict/private/ppict associative-placer<%>)
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
    update-style ;; IStyle -> IStyle

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
    (init-field z style)
    (super-new)

    (define/public (get-z) z)

    (define/public (update-style istyle0)
      (define-values (istyle _ns) (add*-style style istyle0 #:kind 'layer))
      istyle)

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
    (init-field placer  ;; Placer, mainly RefpointPlacer + OverflowPlacer
                zone    ;; Zone, used to update style with width?
                options ;; (Listof Symbol), like 'block-width
                [pre-decorator #f]   ;; Pict -> Pict
                [post-decorator #f]) ;; Pict -> Pict
    (super-new (gap (send placer get-sep)))

    (define zplacer (subplacer placer zone))
    (unless (is-a? placer associative-placer<%>)
      (error 'layer "incompatible placer: ~e" placer))
    (unless (send placer check-associative-vcompose)
      (error 'layer "placer has incompatible compose function: ~e" placer))

    (define/override (update-style istyle)
      (let-values ([(w h x y) (send zone get-zone (blank))])
        (let* ([istyle (hash-set istyle 'layer-width w)]
               [istyle (if (set-width?) (hash-set istyle 'block-width w) istyle)])
          (super update-style istyle))))

    (define/private (set-width?) (memq 'block-width options))

    ;; place : (Listof Pict) LayerPre Pict -> Pict
    (define/override (place ps lpre base)
      (define p (combine-picts ps lpre))
      (send zplacer place base (list p)))

    ;; combine-picts : (Listof Pict) LayerPre -> Pict
    (define/public (combine-picts ps lpre)
      (define-values (p _newsep) (send placer compose-elements ps))
      (define dp (pre-decorate p))
      (post-decorate (inset-to/align dp #f lpre 'ct)))

    (define/public (pre-decorate p)
      (if pre-decorator (pre-decorator p) p))
    (define/public (post-decorate p)
      (if post-decorator (post-decorator p) p))
    ))
