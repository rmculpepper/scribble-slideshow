;; Copyright 2021 Ryan Culpepper.
;; Licensed under the Apache 2.0 license. See LICENSE.

#lang racket/base
(require racket/match
         racket/class
         (only-in slideshow/base
                  current-gap-size margin title-h
                  get-client-h get-client-w)
         pict
         ppict/pict
         ppict/align
         ppict/zone
         "style.rkt")
(provide (all-defined-out))

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
      (inset-to/align p #f lpre (make-align halign 'c)))
    ))

;; ----------------------------------------

(define (layer placer zone
               #:gap [gap 24]
               #:style [style (hasheq)])
  (new layer% (placer placer) (zone zone) (gap gap) (style style)))

(define (next-auto-z)
  (set! auto-z (+ auto-z auto-dz))
  auto-z)

(define auto-z 1.0)
(define auto-dz 0.000001)


;; XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

;; FIXME! To avoid a dependency from layer.rkt to slideshow/base, the layer
;; place method should take a slide-config argument and use that to get
;; client/screen dimensions.

;; Downside: we can't ask a layer for its width (or height) without a
;; slideinfo. (Only if using aspect = #f...) This is okay (maybe?), because we
;; don't *need* the width to update the width until we do the scribble->picts
;; conversion, but it means that all width-dependent things must be delayed!

;; Maybe add an indirection so that named layers (& zones & placers ?) can be
;; registered entirely separately from the document?

#|
(define default-layer%
  (class h-layer-base%
    (inherit get-gap)
    (super-new (z 0)
               (style (hasheq)))

    (define/override (place title? slide-layout ps lpre base)
      ;; FIXME: aspect?
      (define elayout (get-effective-layout title? 'fullscreen slide-layout lpre))
      (define body-p (apply vc-append (current-gap-size) ps))
      (define body-h lpre)
      (define body-w (pict-width body-p))
      (define x (/ (- (pict-width base) body-w) 2))
      (define y (get-client-y title? elayout body-h (pict-height base)))
      (pin-over base x y body-p))
    ))

;; A SlideLayout is 'auto | 'center | 'top | 'tall.
;; An EffectiveLayout is 'center | 'top | 'tall.

;; get-effective-layout : Boolean Aspect SlideLayout Real -> EffectiveLayout
(define (get-effective-layout title? aspect layout body-h)
  (cond [(memq layout '(auto #f))
         (cond [(and title?
                     (> (+ body-h title-h (* 2 (current-gap-size)))
                        (get-client-h #:aspect aspect)))
                'top]
               [else 'center])]
        [else layout]))

;; A LayerLayout is mapped to a RefPage based also on whether there is a title
;; and (possibly) the effective layout of the slide.

;; A LayerLayout is one of
;; - 'slide         -- depend on the slide layout, preferring 'full if no title
;; - 'slide/short   -- depend on the slide layout, preferring 'partial if no title
;; - RefPage        --

(define (layout->refpage llayout title? slide-elayout)
  (case llayout
    [(slide slide/short)
     (if title?
         (case slide-elayout
           [(tall) 't-tall]
           [else #;(top center auto) 't-top])
         (case llayout
           [(slide) 'full]
           [(slide/short) 'partial]))]
    [else #;(full partial t-top t-tall) llayout]))

;; layout = 'slide or 'slide/short
;;   title? = true:
;;     slide-layout = 'top => 't-top + CT placer
;;     slide-layout = 'tall => 't-tall + CT placer
;;     slide-layout = 'center => 't-top + CC placer
;;   title? = false:
;;     layout = 'slide => 'full + CT/CC placer
;;     layout = 'slide/short => 'partial + CT/CC placer

;; FIXME: replace with RefPage, etc
(define (get-client-y title? elayout body-h base-h)
  (case elayout
    [(center) (/ (- base-h body-h) 2)]
    [(top) (if title? (+ title-h (* 2 (current-gap-size))) 0)]
    [(tall) (if title? (+ title-h (* 1 (current-gap-size))) 0)]
    [else (error 'get-client-y "bad effective layout: ~e" elayout)]))

(define default-layer (new default-layer%))
|#

;; A RefPage determines the height and y offset of the "reference page" to which
;; the layer is relative. The y offset is relative to the top of full-page (that
;; is, after the margin is already applied).

;; A RefPage is one of
;; - 'full      -- h = full, y = <center> = 0
;; - 'partial   -- h = titleless, y = <center> = (titleh+2gap)/2
;; - 't-top     -- h = titleless, y = titleh+2gap
;; - 't-tall    -- h = titleless, y = titleh+1gap
(define (refpage-h p)
  (let ([gap (current-gap-size)])
    (case p
      [(full) (get-client-h)]
      [else (- (get-client-h) title-h gap gap)])))
(define (refpage-y p)
  (let ([gap (current-gap-size)])
    (case p
      [(full) 0]
      [(partial) (/ (+ title-h gap gap) 2)]
      [(t-top) (+ title-h gap gap)]
      [(t-tall) (+ title-h gap)]
      [else (error 'refpage-y "bad refpage: ~e" p)])))
