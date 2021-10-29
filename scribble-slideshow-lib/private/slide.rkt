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

;; ----------------------------------------

(define (scribble-slides . pre-parts)
  (scribble-slides* (s:decode pre-parts)))

(define (scribble-slides* p)
  (define-values (h mk) (slides-from-part p #f))
  (void (mk h no-ctx)))

;; ----------------------------------------

;; need W for flow->pict
;; need placer-like to place result
;; for next/alts:
;; - use H, compute ourselves
;; - use placer support?

;; placer args
;; - aspect (wide vs full)
;; - 'tall / 'top / 'full

(struct layer
  (w h        ;; NNReal
   x y        ;; Real (top left corner)
   z          ;; Nat
   style      ;; StyleHash
   valign     ;; (U 'auto 'center 'top 'bottom)
   ) #:prefab)

(define (layer<? a b)
  (let ([za (layer-z a)] [zb (layer-z b)]
        [xa (layer-x a)] [xb (layer-x b)])
    (or (< za zb) (and (= za zb) (< xa xb)))))

;; Heights is Hasheq[Layer => Nat/#f]
(define no-heights (hasheq))

(struct slctx
  (title   ;;
   layout  ;; Layout
   layers  ;; Hasheq[Layer => LayerState]
   ) #:prefab)

;; LayerState = (Listof Pict)

(define no-ctx (slctx #f #f (hasheq)))

(define (slctx-heights ctx)
  (for/hasheq ([(l st) (in-hash (slctx-layers ctx))])
    (values l (layst-height l st))))

(define (layst-height l ps)
  (apply + (add-between (map p:pict-height ps) (p:current-gap-size))))

(define (make-full-layer w h x y [valign 'auto] [istyle (hasheq)])
  (make-layer* 'fullscreen w h x y valign istyle))

(define (make-wide-layer w h x y [valign 'auto] [istyle (hasheq)])
  (make-layer* 'widescreen w h x y valign istyle))

(define (make-layer* aspect w h x y valign istyle)
  (define fw (p:get-client-w #:aspect aspect))
  (define fh (- (p:get-client-w #:aspect aspect) p:title-h (* 2 (p:current-gap-size))))
  (layer (* fw w) (* fh h) (* fw x) (* fh y) 1 istyle valign))

;; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

;; Slide style names:
;; - 'ignore : do not generate slides
;; - 'next : sub-parts successively extend parent
;; - 'alts : sub-parts independently extend parent

;; PRE:
;; - was there ever title?
;; - refpoint layer => max height
;; - nonrefpoint layer => ??? - might care about number, size, or both!
;; - layer => (Listof BlankPict)
;;   - on alts, merge to longest list, max dims per element

(struct preinfo
  (title? ;; Boolean   -- was there ever a title?
   layers ;; Hasheq[Layer/#f => (Listof BlankPict)]
   ) #:prefab)

(define (empty-pre) (preinfo #f (hasheq)))

(define (pre-has-title pre has?)
  (match pre [(preinfo title? layers) (preinfo (or title? has?) layers)]))

(define (pre-update-layer pre lay p)
  (match-define (preinfo title? layers) pre)
  (define bps (blank (p:pict-width p) (p:pict-height p)))
  (preinfo title? (hash-append layers lay (list bp))))

(define (pre-max pre1 pre2)
  (match-define (preinfo title1? layers1) pre1)
  (match-define (preinfo title2? layers2) pre2)
  (define (combine ps1 ps2)
    (match* [ps1 ps2]
      [[ps1 '()] ps1]
      [['() ps2] ps2]
      [[(cons p1 ps1) (cons p2 ps2)]
       (cons (p:blank (max (p:pict-width p1) (p:pict-width p2))
                      (max (p:pict-height p1) (p:pict-height p2)))
             (combine ps1 ps2))]))
  (preinfo (or title1? title2?)
           (hash-union pre1 pre2 #:combine combine)))

(define (hash-append h k vs)
  (hash-set h k (append (hash-ref h k null) vs)))

;; ----------------------------------------

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
                (content->pict title-content istyle +inf.0))]))
    (define layer=>picts
      (for/fold ([layer=>picts ctx-layers])
                ([(lay body-p) (in-hash layer=>pict)])
        (hash-append layer=>picts lay (list body-p))))


    (define-values (layer=>picts layer=>fullpict)
      (for/fold ([layer=>picts (hasheq)] [layer=>fullpict (hasheq)])
                ([(lay layh) (in-hash (preinfo-layers pre))])
        (define body-p (hash-ref layer=>pict lay #f))
        (define ctx-prefix (hash-ref ctx-layers lay null))
        (define full-body (append ctx-prefix (if body-p (list body-p) null)))
        (values (hash-set layer=>picts lay full-body)
                (hash-set layer=>fullpict lay (inset-to-h full-body layh)))))
    (p:slide #:title title-p #:layout 'center #:aspect aspect
             (for/fold ([base (p:get-full-page #:aspect aspect)])
                       ([lay (in-list (sort (hash-keys layer=>picts) layer<?))])
               (define ps (hash-ref layer=>picts lay))
               (cond [lay (add-layer lay ps pre base)]
                     [else (add-default-layer ps pre base)])))
    (slctx title-p layout layer=>picts))
  (values new-h mk))

(define (add-layer lay ps pre base)
  (match-define (prefinfo title? layers) pre)
  (define bps (hash-ref layers lay))
  (define ps* (append ps (drop pbs (length ps))))
  (apply ppict-add (ppict-go base (layer-placer lay)) ps*))

(define (add-default-layer ps pre base)
  ...)

;; split-blocks-by-layer : (Listof Block) -> Hash[Layer/#f => (Listof Block), reversed]
(define (split-blocks-by-layer blocks)
  (define (hash-cons h k v) (hash-set h k (cons v (hash-ref h k null))))
  (define (style-layer style)
    (for/or ([p (in-list (s:style-properties style))] #:when (layer? p)) p))
  (let loop ([h (hasheq)] [blocks blocks] [layer #f])
    (for/fold ([h h]) ([b (in-list blocks)])
      (match b
        [(s:compound-paragraph style blocks)
         (cond [(style-layer style)
                => (lambda (in-layer) (loop h blocks in-layer))]
               [else (hash-cons h layer b)])]
        [b (hash-cons h layer b)]))))

(define (layer-update-style lay istyle)
  (for/fold ([istyle (hash-set istyle 'block-width (layer-w lay))])
            ([(k v) (in-hash (layer-style lay))])
    (hash-set istyle k v)))

(define (get-default-layer/pass2 istyle title? aspect layout)
  (define fw (p:get-client-w #:aspect aspect))
  (define w (get-block-width istyle))
  (define x (/ (- fw w) 2))
  (eprintf "x = ~s, w = ~s, fw = ~s\n" x w fw)
  (define fh (p:get-client-h #:aspect aspect))
  (define (title-yskip gaps)
    (if title? (+ p:title-h (* gaps (p:current-gap-size))) 0))
  (case layout
    [(center) (layer fw fh x 0 0 (hasheq) 'center)]
    [(top) (let ([y (title-yskip 2)])
             (layer fw (- fh y) x y 0 (hasheq) 'top))]
    [(tall) (let ([y (title-yskip 1)])
              (layer fw (- fh y) x y 0 (hasheq) 'top))]
    [else ;; auto
     (cond [title? (let ([yskip (title-yskip 2)])
                     (layer fw (- fh yskip yskip) x yskip 0 (hasheq) 'auto))]
           [else (get-default-layer/pass2 istyle #f aspect 'center)])]))

(define (inset-layer lay body)
  (define w (layer-w lay))
  (define h (layer-h lay))
  (define dw (- w (p:pict-width body)))
  (define dh (- h (p:pict-height body)))
  (when (< dw 0) (eprintf "width overflow: ~s > ~s\n" (p:pict-width body) w))
  (when (< dh 0) (eprintf "height overflow: ~s > ~s\n" (p:pict-height body) h))
  (case (layer-valign lay)
    [(center) (p:inset body 0 (/ dh 2))]
    [(top) (p:inset body 0 0 0 dh)]
    [(bottom) (p:inset body 0 dh 0 0)]
    [else #;(auto) (if (< dh 0) (p:inset body 0 0 0 dh) (p:inset body 0 (/ dh 2)))]))

(define (h-max h1 h2)
  (cond [(and h1 h2) (hash-union h1 h2 #:combine max)]
        [else (or h1 h2)]))

(define (h+ h1 h2)
  (cond [h1 (+ h1 h2 (p:current-gap-size))]
        [else h2]))

(define (inset-to-h ps h)
  (define p (apply p:vc-append (p:current-gap-size) ps))
  (if h (values #;p:frame (p:inset p 0 0 0 (- h (p:pict-height p)))) p))

;; ============================================================

(define (add-slide-style s istyle)
  (match s
    [(s:style name props)
     (foldl add-slide-style-prop (add-slide-style name istyle) props)]
    ;; ----
    ;; standard scribble part styles seem irrelevant, ignore
    [_ istyle]))

(struct make-slides-prop (mk))

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
