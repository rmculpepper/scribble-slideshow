;; Copyright 2020 Ryan Culpepper.
;; Licensed under the Apache 2.0 license. See LICENSE.

#lang racket/base
(require racket/match
         racket/list
         racket/hash
         (prefix-in s: scribble/core)
         (prefix-in s: scribble/html-properties)
         (prefix-in s: scribble/latex-properties)
         (prefix-in s: scribble/decode)
         (prefix-in p: slideshow)
         (prefix-in p: pict)
         (prefix-in p: ppict/pict)
         "pict.rkt")
(provide (all-defined-out))

;; FIXME: title picts of different sizes aren't baseline-aligned,
;; looks inconsistent.

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
  (define-values (h mk) (slides-from-part p #f))
  (void (mk h no-ctx)))

;; ============================================================
;; Layers

;; need W for flow->pict
;; need placer-like to place result
;; for next/alts:
;; - use H, compute ourselves
;; - use placer support?

;; placer args
;; - aspect (wide vs full)
;; - 'tall / 'top / 'full

(struct layer
  (placer   ;; Placer
   z        ;; Real
   style    ;; StyleHash, should set 'block-width
   ) #:prefab)

(define (layer<? a b)
  (let ([za (if a (layer-z a) 0)]
        [zb (if b (layer-z b) 0)])
    (< za zb)))

(define (layer-update-style lay istyle)
  (for/fold ([istyle istyle])
            ([(k v) (in-hash (layer-style lay))])
    (hash-set istyle k v)))

(define auto-z 1.0)
(define auto-dz 0.000001)
(define (next-auto-z)
  (set! auto-z (+ auto-z auto-dz))
  auto-z)

(define (make-layer #:placer placer
                    #:width width
                    #:z [z (next-auto-z)]
                    #:style [istyle (hasheq)])
  (let ([istyle (if width (hash-set istyle 'block-width width) istyle)])
    (layer placer z istyle)))

(define (make-full-layer rx1 rx2 ry align #:style [istyle (hasheq)])
  (define fw (p:get-client-w #:aspect 'fullscreen))
  (make-layer #:placer (p:coord rx1 ry align)
              #:width (* fw (- rx2 rx1))
              #:style istyle))

;; ------------------------------------------------------------
;; Preinfo (slide rendering pass 1)

(struct preinfo
  (title? ;; Boolean   -- was there ever a title?
   layers ;; Hasheq[Layer/#f => (Listof BlankPict)]
   ) #:prefab)

;; Rationale: some placers care about dimensions, others care about
;; number of picts (eg tile), some care about both (eg, cascade).
;; FIXME: this might not be the best compromise...

(define (empty-pre) (preinfo #f (hasheq)))

(define (pre-has-title pre has?)
  (match pre [(preinfo title? layers) (preinfo (or title? has?) layers)]))

(define (pre-update-layer pre lay p)
  (match-define (preinfo title? layers) pre)
  (define bp (p:blank (p:pict-width p) (p:pict-height p)))
  (preinfo title? (hash-append layers lay (list bp))))

(define (pre-max pre1 pre2)
  (match-define (preinfo title1? layers1) (or pre1 (empty-pre)))
  (match-define (preinfo title2? layers2) (or pre2 (empty-pre)))
  (define (combine-max ps1 ps2)
    (match* [ps1 ps2]
      [[ps1 '()] ps1]
      [['() ps2] ps2]
      [[(cons p1 ps1) (cons p2 ps2)]
       (cons (p:blank (max (p:pict-width p1) (p:pict-width p2))
                      (max (p:pict-height p1) (p:pict-height p2)))
             (combine-max ps1 ps2))]))
  (preinfo (or title1? title2?)
           (hash-union layers1 layers2 #:combine combine-max)))

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

;; slides-from-part : Part Pre -> (values Real (Heights SlideContext -> SlideContext))
;; Returns height of body plus slide-maker function.
(define (slides-from-part p ctx-pre)
  (match p
    [(s:part tag-pfx tags title-content style to-collect blocks parts)
     ;; Note: part styles are not inherited.
     (define istyle (add-slide-style style (current-sp-style)))
     (define mk0 (or (hash-ref istyle 'slide-maker #f) void))
     (define-values (pre mk)
       (case (hash-ref istyle 'slide-mode #f)
         [(ignore)
          (values ctx-pre (lambda (pre ctx) ctx))]
         [(next)
          (define-values (pre1 mk1)
            (slide-from-part-contents title-content blocks ctx-pre istyle))
          (for/fold ([pre pre1] [mks (list mk1)]
                     #:result (values pre (do-next (reverse mks))))
                    ([p (in-list parts)])
            (define-values (ppre pmk) (slides-from-part p pre))
            (values ppre (cons pmk mks)))]
         [(alts #f)
          (define-values (pre1 mk1)
            (slide-from-part-contents title-content blocks ctx-pre istyle))
          (for/fold ([pre pre1] [mks (list mk1)]
                     #:result (values pre (do-alts (reverse mks))))
                    ([p (in-list parts)])
            (define-values (ppre pmk) (slides-from-part p ctx-pre))
            (values (pre-max pre ppre) (cons pmk mks)))]))
     (cond [ctx-pre (values pre (lambda (pre ctx) (mk0) (mk pre ctx)))]
           [else (values #f (lambda (_pre ctx) (mk0) (mk pre ctx)))])]))

(define ((do-next mks) pre ctx)
  (for/fold ([ctx ctx]) ([mk (in-list mks)])
    (mk pre ctx)))

(define ((do-alts mks) pre ctx0)
  (for/fold ([ctx ctx0]) ([mk (in-list mks)])
    (mk pre ctx0)))

;; slide-from-part-contents : Content (Listof Block) Pre Style
;;                         -> (values Pre ??)
(define (slide-from-part-contents title-content blocks ctx-pre slide-istyle)
  (define new-pre0
    (pre-has-title (or ctx-pre (empty-pre))
                   (and (member title-content '(#f ())) #t)))
  (define-values (layer=>pict new-h)
    (for/fold ([layer=>pict (hasheq)]
               [new-pre new-pre0])
              ([(lay rblocks) (in-hash (split-blocks-by-layer blocks))])
      (define istyle (if lay (layer-update-style lay slide-istyle) slide-istyle))
      (define body-p (flow->pict (reverse rblocks) istyle))
      (define body-h (p:pict-height body-p))
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
                  (p:inset p 0 (- p:title-h (p:pict-height p)) 0 0)))]))
    (define layer=>picts
      (for/fold ([layer=>picts ctx-layers])
                ([(lay body-p) (in-hash layer=>pict)])
        (hash-append layer=>picts lay (list body-p))))
    (p:slide #:title title-p #:layout 'center #:aspect aspect
             (for/fold ([base (p:get-full-page #:aspect aspect)])
                       ([lay (in-list (sort (hash-keys layer=>picts) layer<?))])
               (match-define (preinfo title? layers) pre)
               (define ps (hash-ref layer=>picts lay))
               (define bps (hash-ref layers lay))
               (define ps* (append ps (drop bps (length ps))))
               (cond [lay (add-layer lay title? ps* base)]
                     [else (add-default-layer title? layout aspect ps* base)])))
    (slctx title-p layout layer=>picts))
  (values new-h mk))

;; split-blocks-by-layer : (Listof Block) -> Hash[Layer/#f => (Listof Block), reversed]
(define (split-blocks-by-layer blocks)
  (define (hash-cons h k v) (hash-set h k (cons v (hash-ref h k null))))
  (define (get-layer style)
    (for/or ([p (in-list (s:style-properties style))] #:when (layer? p)) p))
  (let loop ([h (hasheq)] [blocks blocks] [layer #f])
    (for/fold ([h h]) ([b (in-list blocks)])
      (match b
        [(s:compound-paragraph style blocks)
         (cond [(get-layer style)
                => (lambda (in-layer) (loop h blocks in-layer))]
               [else (hash-cons h layer b)])]
        [b (hash-cons h layer b)]))))

(define (add-layer lay title? ps base)
  (apply p:ppict-add (p:ppict-go base (layer-placer lay)) ps))

(define (add-default-layer title? layout aspect ps base)
  (define gap (p:current-gap-size))
  (define body (apply p:vc-append gap ps))
  (define body-w (p:pict-width body))
  (define body-h (p:pict-height body))
  (define base-w (p:pict-width base))
  (define base-h (p:pict-height base))
  (define title-h p:title-h)
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
  (p:pin-over base x y body))

;; ============================================================

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
