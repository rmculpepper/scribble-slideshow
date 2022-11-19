;; Copyright 2020 Ryan Culpepper.
;; Licensed under the Apache 2.0 license. See LICENSE.

#lang racket/base
(require racket/match
         racket/class
         (prefix-in s: scribble/core)
         (prefix-in s: scribble/decode)
         pict
         ppict/pict
         ppict/zone
         "style.rkt"
         "scribble.rkt"
         "layer.rkt"
         "part.rkt")
(provide (all-defined-out)
         (all-from-out "layer.rkt")
         (all-from-out "part.rkt"))

;; ============================================================
;; Scribble Utils

(define (part/make-slides mk)
  (define s (s:style #f (list 'ignore (make-slides-prop mk))))
  (s:part #f null #f s null null null))

(define (compound* #:layer [layer #f]
                   #:style [s s:plain]
                   . pre-flow)
  (define s*
    (cond [(s:style? s) s]
          [(symbol? s) (s:style s null)]
          [(string? s) (s:style s null)]
          [(eq? s #f) s:plain]
          [else (s:style #f (list s))]))
  (define b (s:compound-paragraph s* (s:decode-flow pre-flow)))
  (cond [layer
         (s:compound-paragraph (s:style 'set-layer (list layer)) (list b))]
        [else b]))


;; ============================================================
;; Slideshow Configuration

;; This module duplicates some basic slideshow settings to avoid introducing a
;; hard dependency on the slideshow module.

(define FS-WIDTH 1024)
(define FS-HEIGHT 768)

(define WS-WIDTH 1360)
(define WS-HEIGHT 766)

(define GAP 24)
(define MARGIN 20)
(define TITLE-H 40)

(define slideshow-config<%>
  (interface ()
    ;; for slide-zone
    slide-zone ;; Symbol Aspect -> Zone

    ;; for ??
    slide/full ;; Pict/#f Aspect Pict -> Void
    emit-slide ;; Pict/#f Aspect Pict -> Void
    ))

(define slideshow-config-base%
  (class object%
    (init-field [margin MARGIN]
                [title-h TITLE-H])
    (super-new)

    (define/public (screenw aspect)
      (case aspect [(widescreen) WS-WIDTH] [else FS-WIDTH]))
    (define/public (screenh aspect)
      (case aspect [(widescreen) WS-HEIGHT] [else FS-HEIGHT]))
    (define/public (get-margin) margin)
    (define/public (titleh) title-h)
    (define/public (gap [n 1]) (* n GAP))
    (define/public (clientw aspect) (- (screenw aspect) margin margin))
    (define/public (clienth aspect) (- (screenh aspect) margin margin))
    (define/public (fullpage) (blank (clientw #f) (clienth #f)))

    ;; ----------------------------------------

    ;; slide-zone : Symbol Aspect -> Zone
    ;; The result assumes that the initial scene is (get-full-page #:aspect #f)
    ;; and will be displayed centered on the screen.
    (define/public (slide-zone name aspect)
      (hash-ref! zone-table (cons name aspect)
                 (lambda () (make-zone (lambda args (slide-zone-f name aspect))))))

    (define zone-table (make-hash))

    ;; slide-zone-f : Symbol Aspect -> (values Real Real Real Real)
    ;; Returns width & height, dx & dy (relative to edges of fullscreen client area).
    (define/public (slide-zone-f name aspect)
      (define (get-screen-dx aspect)
        (if aspect (/ (- (clientw #f) (clientw aspect)) 2) 0))
      (case name
        ;; Vertically centered, title?-independent
        [(main)
         (define dh (+ (titleh) (gap 2)))
         (values (clientw aspect) (- (clienth aspect) dh dh) (get-screen-dx aspect) dh)]
        [(tall-main)
         (define dh (+ (titleh) (gap 1)))
         (values (clientw aspect) (- (clienth aspect) dh dh) (get-screen-dx aspect) dh)]
        [(full)
         (values (clientw aspect) (clienth aspect) (get-screen-dx aspect) 0)]
        [(screen)
         (define margin (get-margin))
         (values (screenw aspect) (screenh aspect) (- (get-screen-dx aspect) margin) (- margin))]
        ;; Non-centered, title?-independent
        [(body)
         (define dh (+ (titleh) (gap 2)))
         (values (clientw aspect) (- (clienth aspect) dh) (get-screen-dx aspect) dh)]
        [(tall-body)
         (define dh (+ (titleh) (gap 1)))
         (values (clientw aspect) (- (clienth aspect) dh) (get-screen-dx aspect) dh)]
        [(title)
         (values (clientw aspect) (titleh) 0 0)]
        [else (error 'slide-zone "unknown slide-zone name: ~e" name)]))

    ;; ----------------------------------------

    ;; slide/full : Pict/#f Aspect Pict -> Void
    (define/public (slide/full title-p aspect page)
      (define (inset-title p) (inset p 0 (- (titleh) (pict-height p)) 0 0))
      (let ([title-p (and title-p (inset-title title-p))])
        (define y (if title-p (- 0 (titleh) (gap)) 0))
        (emit-slide title-p aspect (inset page 0 y 0 0))))

    ;; emit-slide : Pict/#f Aspect Pict -> Void
    (define/public (emit-slide title aspect page)
      (void))
    ))

(define current-slideshow-config
  (make-parameter (new slideshow-config-base%)))

;; slide-zone : Symbol #:aspect Aspect -> Zone
;; The result assumes that the initial scene is (get-full-page #:aspect #f)
;; --- that is, with dimensions (get-client-{w,h} #:aspect #f) --- and will
;; be displayed centered on the screen.
(define (slide-zone name #:aspect [aspect #f])
  (send (current-slideshow-config) slide-zone name aspect))

;; ------------------------------------------------------------

(define picts-slideshow-config%
  (class slideshow-config-base%
    (init-field slides-box)
    (inherit screenw screenh get-margin titleh gap)
    (super-new)

    (define/override (emit-slide title aspect page)
      (define w (screenw aspect))
      (define h (screenh aspect))
      (define margin (get-margin))
      (define base (blank w h))
      (define base+t
        (cond [title
               (let ([tw (pict-width title)] [th (pict-height title)])
                 (pin-over base (* 1/2 (- w tw)) margin title))]
              [else base]))
      (define sp
        (let ([pw (pict-width page)])
          (pin-over base+t (* 1/2 (- w pw)) (+ margin (titleh) (gap)) page)))
      (set-box! slides-box (cons sp (unbox slides-box))))
    ))

;; scribble-slide-picts : Part -> (Listof Pict)
(define (scribble-slide-picts part)
  (define slides-box (box null))
  (scribble-slides/config (new picts-slideshow-config% (slides-box slides-box)) part)
  (reverse (unbox slides-box)))

;; scribble-slides/config : SlideshowConfig Part -> ??
(define (scribble-slides/config config part)
  (parameterize ((current-resolve-info (get-resolve-info (list part))))
    (define renderer
      (new parts-renderer% (config config) (initial-default-layer initial-default-layer)))
    (send renderer render-part (current-sp-style) part)))

;; ------------------------------------------------------------

(module+ slideshow
  (require (only-in slideshow/base slide))
  (provide slideshow-config%
           scribble-slides
           scribble-slides*)

  (define slideshow-config%
    (class slideshow-config-base%
      (super-new)
      (define/override (emit-slide title aspect page)
        (slide page #:title title #:aspect aspect #:layout 'tall))
      ))

  (define (scribble-slides . pre-parts)
    (scribble-slides* (s:decode pre-parts)))

  (define (scribble-slides* part)
    (scribble-slides/config (new slideshow-config%) part)))


;; ============================================================
;; Default placer for 'auto

(define (layer align/placer zone
               #:z [z 1]
               #:style [style #f])
  (define placer
    (cond [(placer? align/placer) align/placer]
          [else (aligned-placer align/placer #:sep GAP)]))
  (define options '(block-width))
  (new layer% (z z) (style style) (placer placer) (zone zone)))

(define (slide-layer align/placer [zone #f]
                     #:aspect [slide-aspect #f]
                     #:base [slide-zone-symbol 'main]
                     #:z [z 1]
                     #:style [style #f])
  (define base-zone (slide-zone slide-zone-symbol #:aspect slide-aspect))
  (layer align/placer (if zone (subzone zone base-zone) base-zone) #:z z #:style style))

(define default-layer%
  (class h-layer-base%
    (inherit-field gap)
    (super-new (z 0) (gap GAP) (style #f))

    (define center-layer (layer #:z 0 (aligned-placer 'cc #:sep gap) (slide-zone 'full)))
    (define t-top-layer (layer #:z 0 (aligned-placer 'ct #:sep gap) (slide-zone 'body)))
    (define t-tall-layer (layer #:z 0 (aligned-placer 'ct #:sep gap) (slide-zone 'tall-body)))
    (define auto-layer (layer #:z 0 (overflow-placer #:sep gap) (slide-zone 'main)))
    (define tl-layer (layer #:z 0 (aligned-placer 'ct #:sep gap) (slide-zone 'full)))

    (define/override (place ps lpre base istyle nstyle)
      (match-define (slide-config title? _ layout) (current-slide-config))
      (define dispatch-lay
        (case layout
          [(center) center-layer]
          [(top) (if title? t-top-layer tl-layer)]
          [(tall) (if title? t-tall-layer tl-layer)]
          [(auto #f) auto-layer]))
      (send dispatch-lay place ps lpre base istyle nstyle))
    ))

(define initial-default-layer (new default-layer%))

;; ============================================================

(module+ main
  (require ppict/slideshow2
           racket/lazy-require
           (only-in slideshow/base slide get-full-page t titlet)
           (submod ".." slideshow))

  (define extra? #f)

  (define (test-slide zname title)
    (define config (slide-config (and title #t) #f #f))
    (define sconf (new slideshow-config%))
    (parameterize ((current-slide-config config))
      (send sconf slide/full (and title (titlet title)) #f
            (ppict-do (frame (get-full-page #:aspect #f))
                      #:go (subplacer (coord 0 0 'cc) (slide-zone zname))
                      (colorize (disk 20) "red")
                      #:go (subplacer (coord 1 1 'cc) (slide-zone zname))
                      (colorize (disk 20) "blue")
                      #:go (subplacer (coord 1/2 1/2 'cc) (slide-zone zname))
                      (t (format "zone: ~e" zname))))))

  (test-slide 'main "main")
  (when extra? (test-slide 'main #f))

  (test-slide 'tall-main "tall-main")
  (when extra? (test-slide 'tall-main #f))

  (test-slide 'full "full")
  (when extra? (test-slide 'full #f))

  (test-slide 'screen "screen")
  (when extra? (test-slide 'screen #f))

  ;; (test-slide 'main/full "main/full")
  ;; (test-slide 'main/full #f)

  (test-slide 'body "body")
  (when extra? (test-slide 'body #f))

  (test-slide 'tall-body "tall-body")
  (when extra? (test-slide 'tall-body #f))

  (test-slide 'title #f)

  (void))
