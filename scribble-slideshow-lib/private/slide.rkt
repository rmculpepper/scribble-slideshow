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

;; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

;; Slide style names:
;; - 'ignore : do not generate slides
;; - 'next : sub-parts successively extend parent
;; - 'alts : sub-parts independently extend parent

(struct make-slides-prop (mk))

;; slides-from-part : Part Heights/#f -> (values Real (Heights SlideContext -> SlideContext))
;; Returns height of body plus slide-maker function.
(define (slides-from-part p ctx-h)
  (match p
    [(s:part tag-pfx tags title-content style to-collect blocks parts)
     ;; Note: part styles are not inherited.
     (let ([istyle (add-slide-style style (current-sp-style))])
       (define mk0 (or (hash-ref istyle 'slide-maker #f) void))
       (define-values (h mk)
         (case (hash-ref istyle 'slide-mode #f)
           [(ignore)
            (values ctx-h (lambda (h ctx) ctx))]
           [(next)
            (define-values (h1 mk1)
              (slide-from-part-contents title-content blocks ctx-h istyle))
            (for/fold ([h h1] [mks (list mk1)]
                       #:result (values h (do-next (reverse mks))))
                      ([p (in-list parts)])
              (define-values (hp mkp) (slides-from-part p h))
              (values hp (cons mkp mks)))]
           [(alts #f)
            (define-values (h1 mk1)
              (slide-from-part-contents title-content blocks ctx-h istyle))
            (for/fold ([h h1] [mks (list mk1)]
                       #:result (values h (do-alts (reverse mks))))
                      ([p (in-list parts)])
              (define-values (hp mkp) (slides-from-part p ctx-h))
              (values (h-max h hp) (cons mkp mks)))]))
       (cond [ctx-h (values h (lambda (h ctx) (mk0) (mk h ctx)))]
             [else (values #f (lambda (_h ctx) (mk0) (mk h ctx)))]))]))

(define ((do-next mks) h ctx)
  (for/fold ([ctx ctx]) ([mk (in-list mks)])
    (mk h ctx)))

(define ((do-alts mks) h ctx0)
  (for/fold ([ctx ctx0]) ([mk (in-list mks)])
    (mk h ctx0)))

;; split-blocks-by-layer : (Listof Block) Layer -> Hash[Layer => (Listof Block)]
(define (split-blocks-by-layer blocks default-layer)
  (define (hash-cons h k v) (hash-set h k (cons v (hash-ref h k null))))
  (let loop ([h (hasheq)] [blocks blocks] [layer default-layer])
    (for/fold ([h h]) ([b (in-list blocks)])
      (match b
        [(s:compound-paragraph style blocks)
         (define in-layer
           (for/or ([p (in-list (s:style-properties style))] #:when (layer? p)) p))
         (if in-layer
             (loop h blocks in-layer)
             (hash-cons h layer b))]
        [b (hash-cons h layer b)]))))

;; slide-from-part-contents : Content (Listof Block) Heights Style
;;                         -> (values Heights 
(define (slide-from-part-contents title-content blocks ctx-h slide-istyle)
  (define default-layer (get-default-layer/pass1 slide-istyle))
  (define-values (layer=>pict new-h)
    (for/fold ([layer=>pict (hasheq)]
               [new-h (or ctx-h (hasheq))])
              ([(l rblocks) (in-hash (split-blocks-by-layer blocks default-layer))])
      (define istyle
        (for/fold ([istyle (hash-set slide-istyle 'block-width (layer-w l))])
                  ([(k v) (in-hash (layer-style l))])
          (hash-set istyle k v)))
      (define body-p (flow->pict (reverse rblocks) istyle))
      (define body-h (p:pict-height body-p))
      (values (hash-set layer=>pict l body-p)
              (hash-set new-h l (h+ (hash-ref new-h l #f) body-h)))))
  (define (mk h ctx)
    (match-define (slctx ctx-title ctx-layout ctx-layers) ctx)
    (define aspect (hash-ref slide-istyle 'slide-aspect #f))
    (define layout (hash-ref slide-istyle 'slide-layout ctx-layout))
    (define title-p
      (match title-content
        [(list "..") ctx-title]
        [(or #f '()) #f]
        [else
         (let ([istyle (get-title-istyle base-istyle)])
           (content->pict title-content istyle +inf.0))]))
    (define default-layer*
      (get-default-layer/pass2 slide-istyle (and title-p #t) aspect layout))
    (define-values (layer=>picts layer=>fullpict)
      (for/fold ([layer=>picts (hasheq)] [layer=>fullpict (hasheq)])
                ([(lay layh) (in-hash h)])
        (define body-p (hash-ref layer=>pict lay #f))
        (define ctx-prefix (hash-ref ctx-layers lay null))
        (define full-body (append ctx-prefix (if body-p (list body-p) null)))
        (values (hash-set layer=>picts lay full-body)
                (hash-set layer=>fullpict lay (inset-to-h full-body layh)))))
    (p:slide #:title title-p #:layout 'center #:aspect aspect
             (for/fold ([base (p:get-full-page #:aspect aspect)])
                       ([lay (in-list (sort (hash-keys layer=>pict) layer<?))])
               (define p (hash-ref layer=>fullpict lay))
               (let ([lay (if (eq? lay default-layer) default-layer* lay)])
                 (p:pin-over base (layer-x lay) (layer-y lay) (inset-layer lay p)))))
    (slctx title-p layout layer=>picts))
  (values new-h mk))

(define (get-default-layer/pass1 istyle)
  (define w (get-block-width istyle))
  (define fh (p:get-client-h))
  (layer w fh 0 0 0 (hasheq) 'center))

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

#;
(define (slide* #:title title-p #:layout layout #:aspect aspect body)
  (case layout
    [(auto center)
     ;; slideshow/core.rkt does vertical centering by using cc-superimpose on
     ;; {full,titleless}-page (search in core.rkt for "(if center?"). That also
     ;; horizontally centers wrt the full screen width, taking control away from
     ;; the slide assembler.
     (define center?
       (or (eq? layout 'center)
           (< (p:pict-height body)
              (- (p:get-client-h #:aspect aspect)
                 (* 2 (+ (* 2 p:gap-size) p:title-h))))))
     (define body*
       (cond [center?
              (define ih (max 0 (- (p:get-client-h #:aspect aspect)
                                   (if title-p (* (+ p:title-h (* 2 p:gap-size))) 0)
                                   (p:pict-height body))))
              (p:inset body 0 (/ ih 2))]
             [else body]))
     (p:slide #:title title-p #:layout 'tall #:aspect aspect body*)]
    [else
     (p:slide #:title title-p #:layout layout #:aspect aspect body)]))

(define (h-max h1 h2)
  (cond [(and h1 h2) (hash-union h1 h2 #:combine max)]
        [else (or h1 h2)]))

(define (h+ h1 h2)
  (cond [h1 (+ h1 h2 (p:current-gap-size))]
        [else h2]))

(define (inset-to-h ps h)
  (define p (apply p:vc-append (p:current-gap-size) ps))
  (if h (values #;p:frame (p:inset p 0 0 0 (- h (p:pict-height p)))) p))

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
  (s:compound-paragraph (s:style #f lay) (s:decode-flow flow)))
