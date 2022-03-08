;; Copyright 2020 Ryan Culpepper.
;; Licensed under the Apache 2.0 license. See LICENSE.

#lang racket/base
(require racket/match
         racket/list
         racket/class
         racket/hash
         (prefix-in s: scribble/core)
         (prefix-in s: scribble/html-properties)
         (prefix-in s: scribble/latex-properties)
         (prefix-in s: scribble/decode)
         (only-in slideshow/base
                  slide get-full-page
                  title-h margin current-gap-size get-client-w get-client-h)
         pict
         ppict/pict
         ppict/zone
         ppict/align
         "style.rkt"
         "content.rkt"
         "block.rkt"
         "scribble.rkt"
         "layer.rkt"
         "part.rkt")
(provide (all-defined-out)
         (all-from-out "layer.rkt")
         (all-from-out "part.rkt"))

;; ============================================================

(define (scribble-slides . pre-parts)
  (scribble-slides* (s:decode pre-parts)))

(define (scribble-slides* p)
  (parameterize ((current-resolve-info (get-resolve-info (list p))))
    (define renderer (new slide-parts-renderer%))
    (send renderer render-part (current-sp-style) p)))

;; ------------------------------------------------------------

(define (part/make-slides mk)
  (define s (s:style #f (list 'ignore (make-slides-prop mk))))
  (s:part #f null #f s null null null))

(define (in-layer #:layer lay . flow)
  (s:compound-paragraph (s:style #f (list lay)) (s:decode-flow flow)))

(define (in-style #:style mstyles . flow)
  (define (add-styles istyle) (merge-styles istyle mstyles))
  (s:compound-paragraph (s:style #f (list (style-transformer add-styles))) (s:decode-flow flow)))

;; ============================================================
;; Slide making

(define slide-parts-renderer%
  (class parts-renderer%
    (inherit compose-page)
    (super-new (initial-default-layer initial-default-layer))

    (define/override (handle-part-blocks istyle sstyles title blocks st)
      (parameterize ((current-slide-config
                      (new slide-config%
                           (title? #t)
                           (aspect (hash-ref sstyles 'aspect #f))
                           (layout (hash-ref sstyles 'layout #f)))))
        (super handle-part-blocks istyle sstyles title blocks st)))

    (define/override (emit-page title-p sstyles st layer=>picts)
      (define layout (hash-ref sstyles 'layout #f))
      (define aspect (hash-ref sstyles 'aspect #f))
      (define (inset-title p) (inset p 0 (- title-h (pict-height p)) 0 0))
      (parameterize ((current-slide-config
                      (new slide-config% (title? (and title-p #t))
                           (aspect aspect) (layout layout))))
        (define page (compose-page (get-full-page #:aspect #f) st layer=>picts))
        (slide/full #:title (and title-p (inset-title title-p)) #:aspect aspect page)))
    ))

;; PRE: page has same dimensions as (get-full-page #:aspect #f).
(define (slide/full #:title title-p #:aspect aspect page)
  (slide #:title title-p #:layout 'tall #:aspect aspect
         (let ([y (if title-p (- 0 title-h (current-gap-size)) 0)])
           (inset page 0 y 0 0))))

;; ============================================================
;; Slide configs and zones

(define slide-config<%>
  (interface ()
    ;; Slide-specific
    slide-title? ;; -> Boolean
    slide-layout ;; -> Layout
    slide-aspect ;; -> Aspect
    ;; Settings
    clientw ;; Aspect -> Real
    clienth ;; Aspect -> Real
    titleh  ;; -> Real
    margin  ;; -> Real
    gap     ;; -> Real
    ))

(define slide-config%
  (class object%
    (init-field title? layout aspect)
    (super-new)
    ;; Slide-specific
    (define/public (slide-title?) title?)
    (define/public (slide-layout) layout)
    (define/public (slide-aspect) aspect)
    ;; Global settings
    (define/public (clientw aspect) (get-client-w #:aspect aspect))
    (define/public (clienth aspect) (get-client-h #:aspect aspect))
    (define/public (titleh) title-h)
    (define/public (get-margin) margin)
    (define/public (gap [n 1]) (* n (current-gap-size)))

    (define/public (screenw aspect)
      (+ (clientw aspect) margin margin))
    (define/public (screenh aspect)
      (+ (clienth aspect) margin margin))
    (define/public (get-screen-dx aspect)
      (if aspect (/ (- (clientw #f) (clientw aspect)) 2) 0))

    (define/public (slide-zone-f name aspect)
      (case name
        ;; Vertically centered, title?-independent
        [(main)
         (define dh (+ title-h (gap 2)))
         (values (clientw aspect) (- (clienth aspect) dh dh) (get-screen-dx aspect) dh)]
        [(tall-main)
         (define dh (+ title-h (gap 1)))
         (values (clientw aspect) (- (clienth aspect) dh dh) (get-screen-dx aspect) dh)]
        [(full)
         (values (clientw aspect) (clienth aspect) (get-screen-dx aspect) 0)]
        [(screen)
         (values (screenw aspect) (screenh aspect) (- (get-screen-dx aspect) margin) (- margin))]
        ;; Vertically centered, title?-dependent
        [(main/full)
         (if (slide-title?)
             (slide-zone-f 'main aspect)
             (slide-zone-f 'full aspect))]
        ;; Non-centered, title?-independent
        [(body)
         (define dh (+ title-h (* 2 (current-gap-size))))
         (values (clientw aspect) (- (clienth aspect) dh) (get-screen-dx aspect) dh)]
        [(tall-body)
         (define dh (+ title-h (* 1 (current-gap-size))))
         (values (clientw aspect) (- (clienth aspect) dh) (get-screen-dx aspect) dh)]
        [(title)
         (values (clientw aspect) title-h 0 0)]
        ;; [(body/client) _]
        ;; [(tall-body/client) _]
        [else (error 'slide-zone "unknown slide-zone name: ~e" name)]))
    ))

(define current-slide-config (make-parameter #f))
(define (get-slide-config who)
  (or (current-slide-config)
      (error who "no slide configuration available")))

;; slide-zone : Symbol #:aspect Aspect -> Zone
;; The result assumes that the initial scene is (get-full-page #:aspect #f)
;; --- that is, with dimensions (get-client-{w,h} #:aspect #f) --- and will
;; be displayed centered on the screen.
(define (slide-zone name #:aspect [aspect #f])
  (define (zone-f . args)
    (send (get-slide-config 'slide-zone) slide-zone-f name aspect))
  (hash-ref! slide-zone-table (cons name aspect)
             (lambda () (make-zone zone-f))))

(define slide-zone-table (make-hash))

;; ============================================================
;; Default placer for 'auto

(define (layer align/placer zone
               #:z [z (next-auto-z)]
               #:style [style (hasheq)])
  (define placer
    (cond [(placer? align/placer) align/placer]
          [else (aligned-placer align/placer #:sep (current-gap-size))]))
  (define options '(block-width))
  (new layer% (z z) (style style) (placer placer) (zone zone) (options options)))

(define default-layer%
  (class h-layer-base%
    (inherit-field gap)
    (super-new (z 0) (gap (current-gap-size)) (style (hasheq)))

    (define center-layer (layer #:z 0 (aligned-placer 'cc #:sep gap) (slide-zone 'main/full)))
    (define t-top-layer (layer #:z 0 (aligned-placer 'ct #:sep gap) (slide-zone 'body)))
    (define t-tall-layer (layer #:z 0 (aligned-placer 'ct #:sep gap) (slide-zone 'tall-body)))
    (define auto-layer (layer #:z 0 (overflow-placer #:sep gap) (slide-zone 'main/full)))
    (define tl-layer (layer #:z 0 (aligned-placer 'ct #:sep gap) (slide-zone 'full)))

    (define/override (place ps lpre base)
      (define conf (get-slide-config 'default-layer))
      (define title? (send conf slide-title?))
      (define dispatch-lay
        (case (send conf slide-layout)
          [(center) center-layer]
          [(top) (if title? t-top-layer tl-layer)]
          [(tall) (if title? t-tall-layer tl-layer)]
          [(auto #f) auto-layer]))
      (send dispatch-lay place ps lpre base))
    ))

(define initial-default-layer (new default-layer%))

;;FIXME: rx ry align #:width ...
(define (make-layer rx1 rx2 ry align
                    #:aspect [aspect 'fullscreen]
                    #:layout [layout 'top]
                    #:gap [gap (current-gap-size)]
                    #:style [style (hasheq)]
                    #:z [z (next-auto-z)])
  (define w (* (get-client-w #:aspect 'fullscreen) (- rx2 rx1)))
  (layer (coord rx1 ry align #:sep gap)
         (slide-zone 'body #:aspect 'fullscreen)
         #:style (hash-set style 'block-width w)))

;; ============================================================

(module+ main
  (require ppict/slideshow2
           (only-in slideshow/base slide get-full-page t))

  (define (test-slide zname title)
    (parameterize ((current-slide-config
                    (new slide-config% (title? (and title #t)) (layout #f) (aspect #f))))
      (slide/full #:title title #:aspect #f
                  (ppict-do (frame (get-full-page #:aspect #f))
                            #:go (subplacer (coord 0 0 'cc) (slide-zone zname))
                            (colorize (disk 20) "red")
                            #:go (subplacer (coord 1 1 'cc) (slide-zone zname))
                            (colorize (disk 20) "blue")
                            #:go (subplacer (coord 1/2 1/2 'cc) (slide-zone zname))
                            (t (format "zone: ~e" zname))))))

  (test-slide 'main "main")
  (test-slide 'main #f)

  (test-slide 'tall-main "tall-main")
  (test-slide 'tall-main #f)

  (test-slide 'full "full")
  (test-slide 'full #f)

  (test-slide 'screen "screen")
  (test-slide 'screen #f)

  (test-slide 'main/full "main/full")
  (test-slide 'main/full #f)

  (test-slide 'body "body")
  (test-slide 'body #f)

  (test-slide 'tall-body "tall-body")
  (test-slide 'tall-body #f)

  (test-slide 'title #f)

  (void))
