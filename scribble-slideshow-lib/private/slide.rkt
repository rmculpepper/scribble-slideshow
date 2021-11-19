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
         "layer.rkt")
(provide (all-defined-out)
         (all-from-out "layer.rkt"))

(define (hash-update* h . kfs)
  (let loop ([h h] [kfs kfs])
    (match kfs
      [(list* k f kfs)
       (loop (hash-update h k f #f) kfs)]
      ['() h])))

(define (hash-append h k vs)
  (hash-set h k (append (hash-ref h k null) vs)))

;; ============================================================

(define (scribble-slides . pre-parts)
  (scribble-slides* (s:decode pre-parts)))

(define (scribble-slides* p)
  (parameterize ((current-resolve-info (get-resolve-info (list p))))
    (define-values (h mk) (slides-from-part p #f))
    (void (mk h no-ctx))))

;; ------------------------------------------------------------

(struct make-slides-prop (mk))

(define (add-slide-style s istyle)
  (match s
    [(s:style name props)
     (foldl add-slide-style-prop (add-slide-style name istyle) props)]
    ;; ----
    ;; standard scribble part styles seem irrelevant, ignore
    [_ istyle]))

(define (add-slide-style-prop prop istyle)
  (match prop
    [(or 'auto 'center 'top 'tall) (hash-set istyle 'slide-layout prop)]
    [(or 'widescreen 'fullscreen) (hash-set istyle 'slide-aspect prop)]
    ['next (hash-set istyle 'slide-mode 'next)]
    ['alts (hash-set istyle 'slide-mode 'alts)]
    ['ignore (hash-set istyle 'slide-mode 'ignore)]
    ['ignore* (hash-set istyle 'slide-mode 'ignore*)]
    [(make-slides-prop mk) (hash-set istyle 'slide-maker mk)]
    ;; ----
    ;; standard scribble part styles seem irrelevant, ignore
    [_ istyle]))

(define (remove-slide-styles istyle)
  (define keys '(slide-layout slide-mode
                 slide-title-color slide-title-size slide-title-base))
  (hash-remove* istyle keys))

(define (get-title-istyle istyle)
  (remove-slide-styles
   (remove-block-styles
    (hash-update* istyle
                  'color (lambda (v) (hash-ref istyle 'slide-title-color v))
                  'text-size (lambda (v) (hash-ref istyle 'slide-title-size v))
                  'text-base (lambda (v) (hash-ref istyle 'slide-title-base v))))))

(define (part/make-slides mk)
  (define s (s:style #f (list 'ignore (make-slides-prop mk))))
  (s:part #f null #f s null null null))

(define (in-layer #:layer lay . flow)
  (s:compound-paragraph (s:style #f (list lay)) (s:decode-flow flow)))

(define (in-style #:style style . flow)
  (define (add-styles istyle)
    (for/fold ([istyle istyle]) ([(k v) (in-hash style)]) (hash-set istyle k v)))
  (s:compound-paragraph (s:style #f (list (style-transformer add-styles))) (s:decode-flow flow)))

;; ============================================================
;; Slide making

;; ------------------------------------------------------------
;; Preinfo (slide rendering pass 1)

(struct preinfo
  (title? ;; Boolean   -- was there ever a title?  -- BROKEN for alts-like !!!
   layers ;; Hasheq[Layer => LayerPre]
   ) #:prefab)

(define (empty-pre) (preinfo #f (hasheq)))

(define (pre-has-title pre has?)
  (match pre [(preinfo title? layers) (preinfo (or title? has?) layers)]))

(define (pre-update-layer pre lay p)
  (match-define (preinfo title? layers) pre)
  (preinfo title? (hash-update layers lay (lambda (lpre) (send lay update-pre lpre p)) #f)))

(define (pre-max pre1 pre2)
  (match-define (preinfo title1? layers1) (or pre1 (empty-pre)))
  (match-define (preinfo title2? layers2) (or pre2 (empty-pre)))
  (preinfo (or title1? title2?)
           (hash-union layers1 layers2
                       #:combine/key (lambda (lay lpre1 lpre2)
                                       (send lay max-pre lpre1 lpre2)))))

;; ------------------------------------------------------------
;; Slide context (slide rendering pass 2)

(struct slctx
  (title   ;;
   layout  ;; Layout
   layers  ;; Hasheq[Layer => (Listof Pict)]
   ) #:prefab)

(define no-ctx (slctx #f #f (hasheq)))

;; ------------------------------------------------------------

;; Slide style names:
;; - 'ignore : do not generate slides
;; - 'next : sub-parts successively extend parent
;; - 'alts : sub-parts independently extend parent

;; Assumption: aspect does not change during 'next/'alts run

;; slides-from-part : Part Pre -> (values Pre/#f (Pre SlideContext -> SlideContext))
;; Returns height of body plus slide-maker function.
(define (slides-from-part p ctx-pre)
  (match p
    [(s:part tag-pfx tags title-content0 style to-collect blocks parts)
     ;; Note: part styles are not inherited.
     (define title-content
       (if (memq 'no-title (s:style-properties style)) #f title-content0))
     (define istyle (add-slide-style style (current-sp-style)))
     (slides-from-part-contents title-content istyle blocks parts ctx-pre)]))

;; slides-from-part-contents : Content Style (Listof Block) (Listof Part) Pre
;;                          -> (values Pre/#f (Pre SlideContext -> SlideContext))
(define (slides-from-part-contents title-content istyle blocks parts ctx-pre)
  (define mk0 (or (hash-ref istyle 'slide-maker #f) void))
  (define-values (pre1 mk1) (slide-from-blocks title-content blocks ctx-pre istyle))
  (define slide-mode (hash-ref istyle 'slide-mode #f))
  (define-values (pre mk)
    (case slide-mode
      [(ignore*)
       (values ctx-pre (lambda (pre ctx) ctx))]
      [(next)
       (for/fold ([pre pre1] [mks (list mk1)]
                  #:result (values pre (do-next (reverse mks))))
                 ([p (in-list parts)])
         (define-values (ppre pmk) (slides-from-part p pre))
         (values ppre (cons pmk mks)))]
      [(alts ignore #f)
       (define ignore? (eq? slide-mode 'ignore))
       (for/fold ([pre (if ignore? ctx-pre pre1)] [mks (if ignore? null (list mk1))]
                  #:result (values pre (do-alts (reverse mks))))
                 ([p (in-list parts)])
         (define-values (ppre pmk) (slides-from-part p ctx-pre))
         (values (pre-max pre ppre) (cons pmk mks)))]))
  (cond [ctx-pre (values pre (lambda (ppre ctx) (mk0) (mk ppre ctx)))]
        [else (values #f (lambda (ppre ctx) (mk0) (mk pre ctx)))]))

(define ((do-next mks) pre ctx)
  (for/fold ([ctx ctx]) ([mk (in-list mks)])
    (mk pre ctx)))

(define ((do-alts mks) pre ctx0)
  (for/fold ([ctx ctx0]) ([mk (in-list mks)])
    (mk pre ctx0)))

;; slide-from-blocks : Content (Listof Block) Pre Style
;;                  -> (values Pre (Pre SlideContext -> SlideContext))
(define (slide-from-blocks title-content blocks ctx-pre slide-istyle)
  (define new-pre0
    (pre-has-title (or ctx-pre (empty-pre))
                   (and (member title-content '(#f ())) #t)))
  (define-values (layer=>pict new-h)
    (for/fold ([layer=>pict (hasheq)]
               [new-pre new-pre0])
              ([(lay rblocks) (in-hash (split-blocks-by-layer blocks))])
      (define istyle (if lay (send lay update-style slide-istyle) slide-istyle))
      (define body-p (flow->pict (reverse rblocks) istyle))
      (values (hash-set layer=>pict lay body-p)
              (pre-update-layer new-pre lay body-p))))
  (define (mk pre ctx)
    (match-define (slctx ctx-title ctx-layout ctx-layers) ctx)
    (define aspect (hash-ref slide-istyle 'slide-aspect #f))
    (define layout (hash-ref slide-istyle 'slide-layout ctx-layout))
    (define title-p
      (match title-content
        [(list "..") ctx-title]
        [(or #f '()) #f]
        [else (let ([istyle (get-title-istyle base-istyle)]) ;; FIXME: use slide-istyle
                (let ([p (content->pict title-content istyle +inf.0)])
                  (inset p 0 (- title-h (pict-height p)) 0 0)))]))
    (define layer=>picts
      (for/fold ([layer=>picts ctx-layers])
                ([(lay body-p) (in-hash layer=>pict)])
        (hash-append layer=>picts lay (list body-p))))
    (define page
      (parameterize ((current-slide-config
                      (new slide-config% (title? (and title-p #t)) (aspect aspect) (layout #f))))
        (for/fold ([base (get-full-page #:aspect #f)])
                  ([lay (in-list (sort (hash-keys layer=>picts) layer<?))])
          (match-define (preinfo title? layers) pre)
          (define ps (hash-ref layer=>picts lay))
          (define lpre (hash-ref layers lay))
          (send lay place ps lpre base))))
    (slide #:title title-p #:layout 'tall #:aspect aspect
           (let ([y (if title-p (- (refpage-y 't-tall)) 0)])
             (inset page 0 y 0 0)))
    (slctx title-p layout layer=>picts))
  (values new-h mk))

;; split-blocks-by-layer : (Listof Block) -> Hash[Layer/#f => (Listof Block), reversed]
(define (split-blocks-by-layer blocks)
  (define (hash-cons h k v) (hash-set h k (cons v (hash-ref h k null))))
  (define (get-layer style)
    (for/or ([p (in-list (s:style-properties style))] #:when (layer? p)) p))
  (let loop ([h (hasheq)] [blocks blocks] [layer default-layer])
    (for/fold ([h h]) ([b (in-list blocks)])
      (match b
        [(s:compound-paragraph style blocks)
         (cond [(get-layer style)
                => (lambda (in-layer) (loop h blocks in-layer))]
               [else (hash-cons h layer b)])]
        [b (hash-cons h layer b)]))))

#;
(define (add-default-layer title? layout aspect ps base)
  (define gap (current-gap-size))
  (define body (apply vc-append gap ps))
  (define body-w (pict-width body))
  (define body-h (pict-height body))
  (define base-w (pict-width base))
  (define base-h (pict-height base))
  (define title-h title-h)
  (define x (/ (- base-w body-w) 2))
  (define y
    (let loop ([layout layout])
      (cond [(eq? layout 'center) (/ (- base-h body-h) 2)]
            [(eq? layout 'top) (if title? (+ title-h gap gap) 0)]
            [(eq? layout 'tall) (if title? (+ title-h gap) 0)]
            ;; 'auto cases:
            [(and title? (> (+ (/ body-h 2) (+ title-h gap gap)) (/ base-h 2)))
             (loop 'top)]
            [else (loop 'center)])))
  (pin-over base x y body))

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

;; FIXME: Move to slide.rkt
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

(require (only-in ppict/private/ppict placer-base% apply-compose)) ;; FIXME!

(define overflow-placer%
  (class placer-base%
    (init-field halign valign overflow-valign sep)
    (super-new)

    (define compose (halign->vcompose halign))
    (define/public (check-associative-vcompose) halign)

    (define/public (compose-elements elems)
      (apply-compose compose sep elems))

    (define/override (place* scene iw ih ix iy elems)
      (define x (+ ix (* iw (align->frac halign))))
      (define-values (newpict newsep) (compose-elements elems))
      (cond [(<= (pict-height newpict) ih)
             (define y (+ iy (* ih (align->frac valign))))
             (pin-over/align scene x y halign valign newpict)]
            [else
             (define y (+ iy (* ih (align->frac overflow-valign))))
             (pin-over/align scene x y halign overflow-valign newpict)]))
    ))

(define (overflow-placer #:halign [halign 'c]
                         #:valign [valign 'c]
                         #:overflow-valign [overflow-valign 't]
                         #:sep [sep 0])
  (new overflow-placer% (halign halign) (valign valign)
       (overflow-valign overflow-valign) (sep sep)))

(define default-layer
  (layer (overflow-placer)
         (slide-zone 'main)))


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
      (slide #:title title #:layout 'tall
             (inset
              (ppict-do (frame (get-full-page #:aspect #f))
                        #:go (subplacer (coord 0 0 'cc) (slide-zone zname))
                        (colorize (disk 20) "red")
                        #:go (subplacer (coord 1 1 'cc) (slide-zone zname))
                        (colorize (disk 20) "blue")
                        #:go (subplacer (coord 1/2 1/2 'cc) (slide-zone zname))
                        (t (format "zone: ~e" zname)))
              0
              (if title (- (+ title-h (* 1 (current-gap-size)))) 0)))))

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
